use crate::envir;
use crate::errors::{Error, ReadError};
use crate::io::FileReader;
use crate::iter::TryIter;
use crate::list::List;
use crate::parser::read_sexpr;
use crate::types::{FuncResult, Lambda, Sexpr, TcoResult};

type EvalResult = Result<Sexpr, Error<Sexpr>>;
type Env = envir::Env<Sexpr>;

pub fn eval(sexpr: &Sexpr, env: &mut Env) -> EvalResult {
    let mut sexpr = sexpr.clone();
    let mut env = env.clone();
    loop {
        match sexpr {
            Sexpr::Symbol(name) => return env.get(&name).ok_or(Error::NotFound(name)),
            Sexpr::List(ref list) => match eval_list(list, &mut env)? {
                (s, None) => return Ok(s),
                (s, Some(e)) => {
                    sexpr = s;
                    env = e;
                }
            },
            _ => return Ok(sexpr),
        }
    }
}

#[inline]
fn eval_list(list: &List<Sexpr>, env: &mut Env) -> TcoResult {
    let head = match list.head() {
        Some(list) => list,
        None => return Ok((Sexpr::null(), None)),
    };
    let args = list.tail().unwrap_or_default();
    match eval(head, env)? {
        Sexpr::Lambda(ref lambda) => lambda.call(&args, env),
        Sexpr::Tco(ref func) => func(&args, env),
        Sexpr::Func(ref func) => match func(&args, env) {
            Ok(sexpr) => Ok((sexpr, None)),
            Err(msg) => Err(msg),
        },
        sexpr => Err(Error::NotCallable(sexpr)),
    }
}

#[inline]
pub fn eval_iter<'a>(
    sexprs: &'a List<Sexpr>,
    env: &'a mut Env,
) -> TryIter<impl Iterator<Item = FuncResult> + 'a, Sexpr, Error<Sexpr>> {
    TryIter {
        iter: sexprs.iter().map(|elem| eval(elem, env)),
        err: None,
    }
}

/// Evaluate all the elements of the list but last, return last element unevaluated
#[inline]
pub fn return_last(args: &List<Sexpr>, env: &mut Env) -> TcoResult {
    let iter = &mut args.iter();
    let mut last = match iter.next() {
        Some(sexpr) => sexpr,
        None => return Ok((Sexpr::Nil, None)),
    };
    for elem in iter {
        eval(last, env)?;
        last = elem;
    }
    Ok((last.clone(), Some(env.clone())))
}

pub fn eval_file(filename: &str, env: &mut Env) -> EvalResult {
    let mut reader = match FileReader::from(filename) {
        Ok(reader) => reader,
        Err(msg) => return Err(Error::ReadError(msg.to_string())),
    };

    let mut last = Sexpr::Nil;
    loop {
        match read_sexpr(&mut reader) {
            Ok(ref sexpr) => last = eval(sexpr, env)?,
            Err(ReadError::EndOfInput) => break,
            Err(msg) => return Err(Error::ReadError(msg.to_string())),
        }
    }
    Ok(last)
}

impl Lambda {
    pub fn from(vars: &List<Sexpr>, body: &List<Sexpr>, env: &mut Env) -> FuncResult {
        let iter = &mut TryIter::new(vars.iter().map(|elem| match elem {
            Sexpr::Symbol(name) => Ok(name),
            sexpr => Err(Error::WrongArg(sexpr.clone())),
        }));
        let vars: Vec<String> = iter.cloned().collect();
        if let Some(Err(err)) = iter.err() {
            return Err(err);
        }
        Ok(Sexpr::Lambda(Box::new(Lambda {
            vars,
            body: body.clone(),
            env: env.clone(),
        })))
    }

    fn call(&self, args: &List<Sexpr>, env: &mut Env) -> TcoResult {
        let local = &mut self.env.branch();
        let vars = &mut self.vars.iter();
        let args = &mut args.iter();

        loop {
            match (vars.next(), args.next()) {
                (Some(var), Some(arg)) => {
                    let val = eval(arg, env)?;
                    local.insert(var, val);
                }
                (None, None) => break,
                _ => return Err(Error::WrongArgNum),
            }
        }

        return_last(&self.body, local)
    }
}

#[cfg(test)]
mod tests {
    use super::{eval, Error};
    use crate::envir;
    use crate::list::List;
    use crate::types::Sexpr;

    #[test]
    fn atoms() {
        let mut env = envir::Env::new();

        assert_eq!(eval(&Sexpr::True, &mut env), Ok(Sexpr::True));
        assert_eq!(eval(&Sexpr::False, &mut env), Ok(Sexpr::False));
        assert_eq!(eval(&Sexpr::Integer(42), &mut env), Ok(Sexpr::Integer(42)));
        assert_eq!(eval(&Sexpr::Float(3.14), &mut env), Ok(Sexpr::Float(3.14)));
        assert_eq!(
            eval(&Sexpr::from("hello world"), &mut env),
            Ok(Sexpr::from("hello world"))
        );
    }

    #[test]
    fn symbols() {
        let mut env = envir::Env::new();
        env.insert(&String::from("true"), Sexpr::True);
        env.insert(&String::from("foo"), Sexpr::symbol("bar"));

        assert_eq!(eval(&Sexpr::symbol("true"), &mut env), Ok(Sexpr::True));
        assert_eq!(
            eval(&Sexpr::symbol("foo"), &mut env),
            Ok(Sexpr::symbol("bar"))
        );
        assert_eq!(
            eval(&Sexpr::symbol("baz"), &mut env),
            Err(Error::NotFound(String::from("baz")))
        );

        let key = Sexpr::symbol("foo");
        assert_eq!(eval(&key, &mut env), Ok(Sexpr::symbol("bar")));
        assert_eq!(eval(&key, &mut env), Ok(Sexpr::symbol("bar")));
    }

    #[test]
    fn null() {
        let mut env = envir::Env::new();
        // () => ()
        assert_eq!(eval(&Sexpr::null(), &mut env), Ok(Sexpr::null()))
    }

    #[test]
    fn not_callable_list() {
        let mut env = envir::Env::new();
        env.insert(&String::from("x"), Sexpr::Integer(42));

        // (x #t #f) => (42 #t #f) => Err
        assert_eq!(
            eval(
                &Sexpr::from(vec![Sexpr::symbol("x"), Sexpr::True, Sexpr::False]),
                &mut env
            ),
            Err(Error::NotCallable(Sexpr::Integer(42)))
        );
    }

    #[test]
    fn simple_function() {
        let mut env = envir::Env::new();
        env.insert(
            &String::from("first"),
            Sexpr::Func(|args, &mut _| Ok(args.head().unwrap().clone())),
        );
        // (fitst 1 2) => 1
        assert_eq!(
            eval(
                &Sexpr::from(vec![
                    Sexpr::symbol("first"),
                    Sexpr::Integer(1),
                    Sexpr::Integer(2)
                ]),
                &mut env
            ),
            Ok(Sexpr::Integer(1))
        );

        // calling it second time should work the same
        assert_eq!(
            eval(
                &Sexpr::from(vec![
                    Sexpr::symbol("first"),
                    Sexpr::Integer(1),
                    Sexpr::Integer(2)
                ]),
                &mut env
            ),
            Ok(Sexpr::Integer(1))
        );
    }

    #[test]
    fn iterator() {
        let mut env = envir::Env::new();
        env.insert(&String::from("a"), Sexpr::Integer(1));
        env.insert(&String::from("b"), Sexpr::Integer(2));

        let list = List::from(vec![
            Sexpr::symbol("a"),
            Sexpr::True,
            Sexpr::symbol("b"),
            Sexpr::Float(3.14),
        ]);
        let mut iter = list.iter().map(|elem| eval(elem, &mut env));
        assert_eq!(iter.next(), Some(Ok(Sexpr::Integer(1))));
        assert_eq!(iter.next(), Some(Ok(Sexpr::True)));
        assert_eq!(iter.next(), Some(Ok(Sexpr::Integer(2))));
        assert_eq!(iter.next(), Some(Ok(Sexpr::Float(3.14))));
        assert_eq!(iter.next(), None);

        // didn't mutate
        assert_eq!(
            list,
            List::from(vec![
                Sexpr::symbol("a"),
                Sexpr::True,
                Sexpr::symbol("b"),
                Sexpr::Float(3.14),
            ])
        );
    }
}
