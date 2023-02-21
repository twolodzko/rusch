use crate::errors::{Error, ReadError};
use crate::parser::read_sexpr;
use crate::reader::FileReader;
use crate::types::{Args, Env, Lambda, Sexpr, TcoResult};

type EvalResult = Result<Sexpr, Error<Sexpr>>;

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
fn eval_list(list: &Args, env: &mut Env) -> TcoResult {
    let head = match list.head() {
        Some(head) => head,
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

/// Evaluate all the elements of the list but last, return last element unevaluated
#[inline]
pub fn eval_but_last(args: &Args, env: &mut Env) -> TcoResult {
    let iter = &mut args.iter();
    let mut last = iter.next().unwrap_or(&Sexpr::Nil);
    for elem in iter {
        eval(last, env)?;
        last = elem;
    }
    Ok((last.clone(), Some(env.clone())))
}

pub fn eval_file(filename: &str, env: &mut Env) -> EvalResult {
    let mut reader = FileReader::from(filename).map_err(|msg| Error::ReadError(msg.to_string()))?;

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
    fn call(&self, args: &Args, env: &mut Env) -> TcoResult {
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

        eval_but_last(&self.body, local)
    }
}

#[cfg(test)]
mod tests {
    use super::{eval, eval_but_last, Error};
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

    #[test]
    fn test_eval_but_last() {
        let env = envir::Env::new();

        let not_eval = Sexpr::Func(|_, _| Err(Error::from("invalid")));

        // returns last value unevaluated
        assert_eq!(
            eval_but_last(
                &List::from(vec![
                    Sexpr::Integer(1),
                    Sexpr::Integer(2),
                    Sexpr::from(vec![not_eval.clone()])
                ]),
                &mut env.clone()
            ),
            Ok((Sexpr::from(vec![not_eval.clone()]), Some(env.clone())))
        );

        let eval_me = Sexpr::Func(|_, _| Err(Error::from("expected error")));

        // stops early on error
        assert_eq!(
            eval_but_last(
                &List::from(vec![
                    Sexpr::Integer(1),
                    Sexpr::Integer(2),
                    Sexpr::from(vec![eval_me]),
                    Sexpr::from(vec![not_eval])
                ]),
                &mut env.clone()
            ),
            Err(Error::from("expected error"))
        )
    }
}
