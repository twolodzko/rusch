use crate::envir;
use crate::errors::Error;
use crate::eval::{eval, eval_file, eval_iter, return_last};
use crate::iter::TryIter;
use crate::list::List;
use crate::types::{FuncResult, Lambda, Sexpr, TcoResult};

type Args = List<Sexpr>;
type Env = envir::Env<Sexpr>;

pub fn root_env() -> Env {
    use crate::types::Sexpr::{Func, Tco};
    Env::from([
        ("-", Func(sub)),
        ("->float", Func(to_float)),
        ("->integer", Func(to_integer)),
        ("*", Func(mul)),
        ("/", Func(div)),
        ("+", Func(add)),
        ("<", Func(lt)),
        ("=", Func(equal)),
        (">", Func(gt)),
        ("and", Func(andfn)),
        ("begin", Tco(begin)),
        ("bool?", Func(is_bool)),
        ("car", Func(car)),
        ("cdr", Func(cdr)),
        ("cond", Tco(condfn)),
        ("cons", Func(cons)),
        ("define", Func(define)),
        ("display", Func(display)),
        ("else", Sexpr::True),
        ("eq?", Func(equal)),
        ("equal?", Func(equal)),
        ("error", Func(to_error)),
        ("eval", Func(evalfn)),
        ("float?", Func(is_float)),
        ("if", Tco(iffn)),
        ("integer?", Func(is_integer)),
        ("lambda", Func(lambda)),
        ("let", Tco(let_core)),
        ("let*", Tco(let_star)),
        ("list", Func(list)),
        ("load", Func(load)),
        ("load", Func(load)),
        ("not", Func(not)),
        ("null?", Func(is_null)),
        ("number?", Func(is_number)),
        ("or", Func(orfn)),
        ("pair?", Func(is_pair)),
        ("procedure?", Func(is_procedure)),
        ("quote", Func(quote)),
        ("set!", Func(set)),
        ("string?", Func(is_string)),
        ("string", Func(to_string)),
        ("symbol?", Func(is_symbol)),
    ])
}

fn quote(args: &Args, _env: &mut Env) -> FuncResult {
    if args.has_next() {
        return Err(Error::WrongArgNum);
    }
    args.head().cloned().ok_or(Error::WrongArgNum)
}

/// Evaluate first argument, raise error if more arguments were passed
#[inline]
fn eval_one_arg(args: &Args, env: &mut Env) -> FuncResult {
    if args.has_next() {
        return Err(Error::WrongArgNum);
    }
    match args.head() {
        Some(sexpr) => eval(sexpr, env),
        None => Err(Error::WrongArgNum),
    }
}

fn car(args: &Args, env: &mut Env) -> FuncResult {
    match eval_one_arg(args, env)? {
        Sexpr::List(list) => list.head().cloned().ok_or(Error::WrongArg(Sexpr::null())),
        sexpr => Err(Error::WrongArg(sexpr)),
    }
}

fn cdr(args: &Args, env: &mut Env) -> FuncResult {
    match eval_one_arg(args, env)? {
        Sexpr::List(list) => {
            if list.is_empty() {
                return Err(Error::WrongArg(Sexpr::List(list)));
            };
            Ok(Sexpr::List(list.tail().unwrap_or_default()))
        }
        sexpr => Err(Error::WrongArg(sexpr)),
    }
}

fn cons(args: &Args, env: &mut Env) -> FuncResult {
    // (cons lhs rhs)
    let iter = &mut eval_iter(args, env);
    let lhs = iter.next();
    let rhs = iter.next();
    if let Some(err) = iter.err() {
        return err;
    }
    if iter.next().is_some() {
        return Err(Error::WrongArgNum);
    }
    let list = match rhs {
        Some(Sexpr::List(list)) => list,
        Some(sexpr) => List::from_element(sexpr),
        None => return Err(Error::WrongArgNum),
    };
    Ok(Sexpr::List(list.push_front(lhs.unwrap())))
}

fn list(args: &Args, env: &mut Env) -> FuncResult {
    let iter = &mut eval_iter(args, env);
    let list: List<Sexpr> = iter.collect();
    match iter.err() {
        Some(Err(msg)) => Err(msg),
        _ => Ok(Sexpr::List(list)),
    }
}

fn begin(args: &Args, env: &mut Env) -> TcoResult {
    return_last(args, env)
}

fn lambda(args: &Args, env: &mut Env) -> FuncResult {
    match args.head() {
        Some(Sexpr::List(ref list)) => {
            let iter = &mut TryIter::new(list.iter().map(|elem| match elem {
                Sexpr::Symbol(ref name) => Ok(name.clone()),
                sexpr => Err(Error::WrongArg(sexpr.clone())),
            }));
            let vars: Vec<std::string::String> = iter.collect();
            match iter.err() {
                Some(Err(err)) => Err(err),
                Some(_) => unreachable!(),
                None => Ok(Sexpr::Lambda(Lambda::new(
                    &vars,
                    &args.tail().unwrap_or_default(),
                    &env.clone(),
                ))),
            }
        }
        Some(sexpr) => Err(Error::WrongArg(sexpr.clone())),
        None => Err(Error::WrongArgNum),
    }
}

fn not(args: &Args, env: &mut Env) -> FuncResult {
    if args.is_empty() {
        return Ok(Sexpr::False);
    }
    Ok(Sexpr::from(!eval_one_arg(args, env)?.is_true()))
}

fn andfn(args: &Args, env: &mut Env) -> FuncResult {
    let iter = &mut eval_iter(args, env);
    for elem in &mut *iter {
        if !elem.is_true() {
            return iter.err().unwrap_or(Ok(Sexpr::False));
        }
    }
    iter.err().unwrap_or(Ok(Sexpr::True))
}

fn orfn(args: &Args, env: &mut Env) -> FuncResult {
    let iter = &mut eval_iter(args, env);
    for elem in &mut *iter {
        if elem.is_true() {
            return iter.err().unwrap_or(Ok(Sexpr::True));
        }
    }
    iter.err().unwrap_or(Ok(Sexpr::False))
}

fn add(args: &Args, env: &mut Env) -> FuncResult {
    let iter = &mut eval_iter(args, env);
    let mut acc = Sexpr::Integer(0);
    for elem in &mut *iter {
        acc = (acc + elem)?;
    }
    iter.err().unwrap_or(Ok(acc))
}

fn sub(args: &Args, env: &mut Env) -> FuncResult {
    use crate::types::Sexpr::{Float, Integer};
    let mut iter = args.iter();

    let init = match iter.next() {
        Some(sexpr) => eval(sexpr, env)?,
        None => return Ok(Integer(0)),
    };
    if !init.is_number() {
        return Err(Error::NotANumber(init));
    }
    if !args.has_next() {
        return match init {
            Integer(x) => Ok(Integer(-x)),
            Float(x) => Ok(Float(-x)),
            _ => unreachable!(),
        };
    }

    let iter = &mut TryIter::new(iter.map(|elem| eval(elem, env)));
    let mut acc = init;
    for elem in &mut *iter {
        acc = (acc - elem)?;
    }
    iter.err().unwrap_or(Ok(acc))
}

fn mul(args: &Args, env: &mut Env) -> FuncResult {
    let iter = &mut eval_iter(args, env);
    let mut acc = Sexpr::Integer(1);
    for elem in &mut *iter {
        acc = (acc * elem)?;
    }
    iter.err().unwrap_or(Ok(acc))
}

fn div(args: &Args, env: &mut Env) -> FuncResult {
    let mut iter = args.iter();

    let init = match iter.next() {
        Some(sexpr) => eval(sexpr, env)?,
        None => return Err(Error::WrongArgNum),
    };
    if !init.is_number() {
        return Err(Error::NotANumber(init));
    }
    if !args.has_next() {
        return Sexpr::Float(1.0) / init;
    }

    let iter = &mut TryIter::new(iter.map(|elem| eval(elem, env)));
    let mut acc = init;
    for elem in &mut *iter {
        acc = (acc / elem)?;
    }
    iter.err().unwrap_or(Ok(acc))
}

fn equal(args: &Args, env: &mut Env) -> FuncResult {
    let iter = &mut eval_iter(args, env);
    let mut result = Sexpr::True;

    let mut prev;
    match iter.next() {
        Some(sexpr) => prev = sexpr,
        None => return Ok(result),
    }

    for elem in &mut *iter {
        if elem != prev {
            result = Sexpr::False;
            break;
        }
        prev = elem;
    }
    iter.err().unwrap_or(Ok(result))
}

#[inline]
fn cmp(args: &Args, env: &mut Env, order: std::cmp::Ordering) -> FuncResult {
    let iter = &mut eval_iter(args, env);
    let mut result = Sexpr::True;

    let mut prev = match iter.next() {
        Some(sexpr) => sexpr,
        None => return Ok(result),
    };

    for elem in &mut *iter {
        match prev.partial_cmp(&elem) {
            Some(cmp) if cmp == order => prev = elem,
            Some(_) => {
                result = Sexpr::False;
                break;
            }
            None => return Err(Error::NotANumber(elem)),
        };
    }
    iter.err().unwrap_or(Ok(result))
}

fn gt(args: &Args, env: &mut Env) -> FuncResult {
    cmp(args, env, std::cmp::Ordering::Greater)
}

fn lt(args: &Args, env: &mut Env) -> FuncResult {
    cmp(args, env, std::cmp::Ordering::Less)
}

fn iffn(args: &Args, env: &mut Env) -> TcoResult {
    let mut iter = args.iter();
    match iter.next() {
        Some(sexpr) => {
            if !eval(sexpr, env)?.is_true() {
                iter.next();
            }
        }
        None => return Err(Error::WrongArgNum),
    }
    match iter.next() {
        Some(sexpr) => Ok((sexpr.clone(), Some(env.clone()))),
        None => Err(Error::WrongArgNum),
    }
}

fn condfn(args: &Args, env: &mut Env) -> TcoResult {
    for branch in args.iter() {
        match branch {
            Sexpr::List(list) => match list.head() {
                Some(sexpr) => {
                    let condition = eval(sexpr, env)?;
                    if condition.is_true() {
                        match list.tail() {
                            Some(ref body) => return return_last(body, env),
                            None => return Ok((condition, None)),
                        }
                    }
                }
                None => return Err(Error::WrongArgNum),
            },
            sexpr => return Err(Error::WrongArg(sexpr.clone())),
        }
    }
    Ok((Sexpr::Nil, None))
}

#[inline]
fn bind(args: &Args, eval_env: &mut Env, save_env: &mut Env) -> FuncResult {
    let mut iter = args.iter();

    let key = match iter.next() {
        Some(Sexpr::Symbol(name)) => name,
        Some(sexpr) => return Err(Error::WrongArg(sexpr.clone())),
        None => return Err(Error::WrongArgNum),
    };

    match iter.next() {
        Some(sexpr) => {
            if iter.next().is_some() {
                // has third arg
                return Err(Error::WrongArgNum);
            }
            let val = eval(sexpr, eval_env)?;
            save_env.insert(key, &val);
            Ok(val)
        }
        // missing second arg
        None => Err(Error::WrongArgNum),
    }
}

fn define(args: &Args, env: &mut Env) -> FuncResult {
    bind(args, &mut env.clone(), &mut env.clone())
}

fn set(args: &Args, env: &mut Env) -> FuncResult {
    let mut iter = args.iter();

    let key = match iter.next() {
        Some(Sexpr::Symbol(name)) => name,
        Some(sexpr) => return Err(Error::WrongArg(sexpr.clone())),
        None => return Err(Error::WrongArgNum),
    };

    let val = match iter.next() {
        Some(sexpr) => eval(sexpr, env)?,
        // missing second arg
        None => return Err(Error::WrongArgNum),
    };
    if iter.next().is_some() {
        // has third arg
        return Err(Error::WrongArgNum);
    }

    match env.find_env(key) {
        Some(ref mut local) => {
            local.insert(key, &val);
            Ok(val)
        }
        None => Err(Error::NotFound(key.clone())),
    }
}

fn let_impl(args: &Args, call_env: &mut Env, eval_env: &mut Env) -> TcoResult {
    match args.head() {
        Some(Sexpr::List(ref list)) => {
            let iter = &mut TryIter::new(list.iter().map(|elem| match elem {
                Sexpr::List(ref binding) => bind(binding, call_env, eval_env),
                sexpr => Err(Error::WrongArg(sexpr.clone())),
            }));
            iter.last(); // iterate through all the elements
            if let Some(Err(msg)) = iter.err() {
                return Err(msg);
            }
        }
        Some(sexpr) => return Err(Error::WrongArg(sexpr.clone())),
        None => return Err(Error::WrongArgNum),
    }

    return_last(&args.tail().unwrap_or_default(), eval_env)
}

fn let_core(args: &Args, env: &mut Env) -> TcoResult {
    let local = &mut env.branch();
    let_impl(args, env, local)
}

fn let_star(args: &Args, env: &mut Env) -> TcoResult {
    let local = &mut env.branch();
    let_impl(args, &mut local.clone(), local)
}

fn is_symbol(args: &Args, env: &mut Env) -> FuncResult {
    Ok(Sexpr::from(matches!(
        eval_one_arg(args, env)?,
        Sexpr::Symbol(_)
    )))
}

fn is_bool(args: &Args, env: &mut Env) -> FuncResult {
    Ok(Sexpr::from(matches!(
        eval_one_arg(args, env)?,
        Sexpr::True | Sexpr::False
    )))
}

fn is_string(args: &Args, env: &mut Env) -> FuncResult {
    Ok(Sexpr::from(matches!(
        eval_one_arg(args, env)?,
        Sexpr::String(_)
    )))
}

fn is_number(args: &Args, env: &mut Env) -> FuncResult {
    Ok(Sexpr::from(matches!(
        eval_one_arg(args, env)?,
        Sexpr::Integer(_) | Sexpr::Float(_)
    )))
}

fn is_integer(args: &Args, env: &mut Env) -> FuncResult {
    Ok(Sexpr::from(matches!(
        eval_one_arg(args, env)?,
        Sexpr::Integer(_)
    )))
}

fn is_float(args: &Args, env: &mut Env) -> FuncResult {
    Ok(Sexpr::from(matches!(
        eval_one_arg(args, env)?,
        Sexpr::Float(_)
    )))
}

fn is_procedure(args: &Args, env: &mut Env) -> FuncResult {
    Ok(Sexpr::from(matches!(
        eval_one_arg(args, env)?,
        Sexpr::Func(_) | Sexpr::Lambda(_) | Sexpr::Tco(_)
    )))
}

fn is_pair(args: &Args, env: &mut Env) -> FuncResult {
    match eval_one_arg(args, env)? {
        Sexpr::List(ref list) => Ok(Sexpr::from(!list.is_empty())),
        _ => Ok(Sexpr::False),
    }
}

fn is_null(args: &Args, env: &mut Env) -> FuncResult {
    match eval_one_arg(args, env)? {
        Sexpr::List(ref list) => Ok(Sexpr::from(list.is_empty())),
        _ => Ok(Sexpr::False),
    }
}

fn to_integer(args: &Args, env: &mut Env) -> FuncResult {
    let sexpr = eval_one_arg(args, env)?;
    match &sexpr {
        Sexpr::Integer(_) => Ok(sexpr),
        Sexpr::Float(number) => Ok(Sexpr::Integer(*number as i64)),
        Sexpr::String(string) => match string.parse::<i64>() {
            Ok(number) => Ok(Sexpr::Integer(number)),
            Err(_) => Err(Error::CannotParse(sexpr)),
        },
        _ => Err(Error::CannotParse(sexpr)),
    }
}

fn to_float(args: &Args, env: &mut Env) -> FuncResult {
    let sexpr = eval_one_arg(args, env)?;
    match &sexpr {
        Sexpr::Float(_) => Ok(sexpr),
        Sexpr::Integer(number) => Ok(Sexpr::Float(*number as f64)),
        Sexpr::String(string) => match string.parse::<f64>() {
            Ok(number) => Ok(Sexpr::Float(number)),
            Err(_) => Err(Error::CannotParse(sexpr)),
        },
        _ => Err(Error::CannotParse(sexpr)),
    }
}

fn to_string(args: &Args, env: &mut Env) -> FuncResult {
    let iter = &mut eval_iter(args, env);
    let string = iter
        .map(|elem| elem.to_string())
        .collect::<Vec<String>>()
        .join(" ");
    match iter.err() {
        Some(err) => err,
        None => Ok(Sexpr::String(string)),
    }
}

fn to_error(args: &Args, env: &mut Env) -> FuncResult {
    // TODO: refactor later
    match to_string(args, env) {
        Ok(Sexpr::String(msg)) => Err(Error::Custom(msg)),
        Ok(_) => unreachable!(),
        err => err,
    }
}

fn evalfn(args: &Args, env: &mut Env) -> FuncResult {
    if args.has_next() {
        return Err(Error::WrongArgNum);
    }
    match args.head() {
        Some(sexpr) => eval(&eval(sexpr, env)?, env),
        None => Err(Error::WrongArgNum),
    }
}

fn load(args: &Args, env: &mut Env) -> FuncResult {
    if args.is_empty() {
        return Err(Error::WrongArgNum);
    }

    let mut last = Sexpr::Nil;
    for arg in args.iter() {
        match arg {
            Sexpr::String(ref filename) => {
                last = eval_file(filename, env)?;
            }
            sexpr => return Err(Error::WrongArg(sexpr.clone())),
        }
    }
    Ok(last)
}

fn display(args: &Args, env: &mut Env) -> FuncResult {
    match to_string(args, env)? {
        Sexpr::String(s) => println!("{}", s),
        _ => unreachable!(),
    };
    Ok(Sexpr::Nil)
}

#[cfg(test)]
mod tests {
    use super::root_env;
    use crate::errors::Error;
    use crate::eval::eval;
    use crate::parser::read_sexpr;
    use crate::reader::StringReader;
    use crate::types::Sexpr;

    #[macro_export]
    macro_rules! parse_eval {
        ( $txt:expr, $env:expr ) => {{
            let sexpr = read_sexpr(&mut StringReader::from($txt)).unwrap();
            eval(&sexpr, $env)
        }};
    }

    /// Assert if expression is equal to the string representation
    #[macro_export]
    macro_rules! assert_eval_eq {
        ( $lhs:expr, $rhs:expr ) => {
            let env = &mut root_env();
            assert_eq!(parse_eval!($lhs, env), $rhs);
        };
    }

    #[test]
    fn quote() {
        assert_eval_eq!("(quote foo)", Ok(Sexpr::symbol("foo")));

        assert_eval_eq!("(quote)", Err(Error::WrongArgNum));
        assert_eval_eq!("(quote foo bar)", Err(Error::WrongArgNum));
    }

    #[test]
    fn car() {
        assert_eval_eq!("(car '())", Err(Error::WrongArg(Sexpr::null())));
        assert_eval_eq!("(car '(1 2 3))", Ok(Sexpr::Integer(1)));

        assert_eval_eq!("(car)", Err(Error::WrongArgNum));
        assert_eval_eq!("(car #t)", Err(Error::WrongArg(Sexpr::True)));
    }

    #[test]
    fn cdr() {
        assert_eval_eq!("(cdr '())", Err(Error::WrongArg(Sexpr::null())));
        assert_eval_eq!("(cdr '(1))", Ok(Sexpr::null()));
        assert_eval_eq!(
            "(cdr '(1 2 3))",
            Ok(Sexpr::from(vec![Sexpr::Integer(2), Sexpr::Integer(3)]))
        );

        assert_eval_eq!("(cdr)", Err(Error::WrongArgNum));
        assert_eval_eq!("(cdr #t)", Err(Error::WrongArg(Sexpr::True)));
    }

    #[test]
    fn cons() {
        use crate::list::List;

        assert_eval_eq!(
            "(cons 1 '())",
            Ok(Sexpr::List(List::from_element(Sexpr::Integer(1))))
        );
        assert_eval_eq!(
            "(cons '() '())",
            Ok(Sexpr::List(List::from_element(Sexpr::null())))
        );
        assert_eval_eq!(
            "(cons 1 2)",
            Ok(Sexpr::from(vec![Sexpr::Integer(1), Sexpr::Integer(2)]))
        );
        assert_eval_eq!(
            "(cons 1 '(2 3))",
            Ok(Sexpr::from(vec![
                Sexpr::Integer(1),
                Sexpr::Integer(2),
                Sexpr::Integer(3)
            ]))
        );
        assert_eval_eq!(
            "(cons 1 (list 2 3))",
            Ok(Sexpr::from(vec![
                Sexpr::Integer(1),
                Sexpr::Integer(2),
                Sexpr::Integer(3)
            ]))
        );

        // errors
        assert_eval_eq!("(cons 1)", Err(Error::WrongArgNum));
        assert_eval_eq!("(cons 1 '() '())", Err(Error::WrongArgNum));
    }

    #[test]
    fn list() {
        assert_eval_eq!("(list)", Ok(Sexpr::null()));
        assert_eval_eq!(
            "(list 1 2 3)",
            Ok(Sexpr::from(vec![
                Sexpr::Integer(1),
                Sexpr::Integer(2),
                Sexpr::Integer(3)
            ]))
        );
        assert_eval_eq!(
            "(list 'foo (list 1 2 3))",
            Ok(Sexpr::from(vec![
                Sexpr::symbol("foo"),
                Sexpr::from(vec![
                    Sexpr::Integer(1),
                    Sexpr::Integer(2),
                    Sexpr::Integer(3)
                ])
            ]))
        );
        assert_eval_eq!("(list 'foo (list 1 (car) 3))", Err(Error::WrongArgNum));
    }

    #[test]
    fn add() {
        use Sexpr::{Float, Integer};

        // basics
        assert_eval_eq!("(+)", Ok(Integer(0)));
        assert_eval_eq!("(+ 2 2)", Ok(Integer(4)));
        assert_eval_eq!("(+ '2 2)", Ok(Integer(4)));
        assert_eval_eq!("(+ 1 2 3)", Ok(Integer(6)));
        assert_eval_eq!("(+ 1 (+ 1 1) (+ 1 1 1))", Ok(Integer(6)));

        // floats & casts
        assert_eval_eq!("(+ 2.0  2.0 )", Ok(Float(4.0)));
        assert_eval_eq!("(+  2   2.0 )", Ok(Float(4.0)));
        assert_eval_eq!("(+ 2.0   2  )", Ok(Float(4.0)));

        // errors
        assert_eval_eq!("(+ 1 'foo)", Err(Error::NotANumber(Sexpr::symbol("foo"))));
        assert_eval_eq!("(+ \"1\" 2 3 4)", Err(Error::NotANumber(Sexpr::from("1"))));
    }

    #[test]
    fn sub() {
        use Sexpr::{Float, Integer};

        // basics
        assert_eval_eq!("(-)", Ok(Integer(0)));
        assert_eval_eq!("(- 1)", Ok(Integer(-1)));
        assert_eval_eq!("(- 3 2)", Ok(Integer(1)));
        assert_eval_eq!("(- '3 2)", Ok(Integer(1)));
        assert_eval_eq!("(- 10 5 2)", Ok(Integer(3)));
        assert_eval_eq!("(- 6 (- 2 1) (- 6 3 1))", Ok(Integer(3)));

        // floats & casts
        assert_eval_eq!("(- 2.0  1.0 )", Ok(Float(1.0)));
        assert_eval_eq!("(-  2   1.0 )", Ok(Float(1.0)));
        assert_eval_eq!("(- 2.0   1  )", Ok(Float(1.0)));

        // errors
        assert_eval_eq!("(- 1 'foo)", Err(Error::NotANumber(Sexpr::symbol("foo"))));
        assert_eval_eq!("(- \"1\" 2 3 4)", Err(Error::NotANumber(Sexpr::from("1"))));
    }

    #[test]
    fn mul() {
        use Sexpr::{Float, Integer};

        // basics
        assert_eval_eq!("(*)", Ok(Integer(1)));
        assert_eval_eq!("(* 2 3)", Ok(Integer(6)));
        assert_eval_eq!("(* '2 3)", Ok(Integer(6)));
        assert_eval_eq!("(* 1 2 3)", Ok(Integer(6)));
        assert_eval_eq!("(* 1 (* 1 2) (* 1 2 3))", Ok(Integer(12)));

        // floats & casts
        assert_eval_eq!("(* 2.0  3.0 )", Ok(Float(6.0)));
        assert_eval_eq!("(*  2   3.0 )", Ok(Float(6.0)));
        assert_eval_eq!("(* 2.0   3  )", Ok(Float(6.0)));

        // errors
        assert_eval_eq!("(* 1 'foo)", Err(Error::NotANumber(Sexpr::symbol("foo"))));
        assert_eval_eq!("(* \"1\" 2 3 4)", Err(Error::NotANumber(Sexpr::from("1"))));
    }

    #[test]
    fn div() {
        use Sexpr::Float;

        // basics
        assert_eval_eq!("(/)", Err(Error::WrongArgNum));
        assert_eval_eq!("(/ 2)", Ok(Float(0.5)));
        assert_eval_eq!("(/ '9 3)", Ok(Float(3.0)));
        assert_eval_eq!("(/ 30 10 3)", Ok(Float(1.0)));
        assert_eval_eq!("(/ 300 (/ 12 4) (/ 500 5 10))", Ok(Float(10.0)));

        // // floats & casts
        assert_eval_eq!("(/ 6.0  3.0 )", Ok(Float(2.0)));
        assert_eval_eq!("(/  6   3.0 )", Ok(Float(2.0)));
        assert_eval_eq!("(/ 6.0   3  )", Ok(Float(2.0)));

        // errors
        assert_eval_eq!("(/ 1 'foo)", Err(Error::NotANumber(Sexpr::symbol("foo"))));
        assert_eval_eq!("(/ \"1\" 2 3 4)", Err(Error::NotANumber(Sexpr::from("1"))));
    }

    #[test]
    fn is_equal() {
        use crate::types::Sexpr::{False, True};

        // positive
        assert_eval_eq!("(equal?)", Ok(True));
        assert_eval_eq!("(equal? #t)", Ok(True));
        assert_eval_eq!("(equal? #f)", Ok(True));
        assert_eval_eq!("(equal? 2 2)", Ok(True));
        assert_eval_eq!("(equal? '() '())", Ok(True));
        assert_eval_eq!("(equal? '() (list) (cdr '('head)))", Ok(True));
        assert_eval_eq!("(equal? '(1 2 3) (list 1 (+ 1 1) (+ 1 2)))", Ok(True));
        assert_eval_eq!("(equal? 2 2.0 (+ 1 1) (/ 6 3))", Ok(True));

        // negative
        assert_eval_eq!("(equal? 1 2)", Ok(False));
        assert_eval_eq!("(equal? 2 \"2\")", Ok(False));
        assert_eval_eq!("(equal? \"foo\" 'foo)", Ok(False));
        assert_eval_eq!("(equal? (list 1 2 3) (list 1 2))", Ok(False));
        assert_eval_eq!("(equal? (list 1 2 3) (list 1 2 3 4))", Ok(False));
    }

    #[test]
    fn cmp() {
        use crate::types::Sexpr::{False, True};

        // positive
        assert_eval_eq!("(<)", Ok(True));
        assert_eval_eq!("(< 1)", Ok(True));
        assert_eval_eq!("(< 1 2)", Ok(True));
        assert_eval_eq!("(< 1 2 3)", Ok(True));
        assert_eval_eq!("(< 1 (+ 1 2) (+ 1 (+ 1 2)))", Ok(True));

        assert_eval_eq!("(>)", Ok(True));
        assert_eval_eq!("(> 1)", Ok(True));
        assert_eval_eq!("(> 3 2 1)", Ok(True));

        // negative
        assert_eval_eq!("(< 1 3 2)", Ok(False));
        assert_eval_eq!("(> 3 1 2)", Ok(False));
        assert_eval_eq!("(< 1 2 3 2 5)", Ok(False));
    }

    #[test]
    fn iffn() {
        use crate::types::Sexpr::*;

        assert_eval_eq!("(if #t 1 2)", Ok(Integer(1)));
        assert_eval_eq!("(if #f 1 2)", Ok(Integer(2)));
        assert_eval_eq!("(if #t (+ 1 2) (+ 3 4))", Ok(Integer(3)));
        assert_eval_eq!("(if #f (+ 1 2) (+ 3 4))", Ok(Integer(7)));
        assert_eval_eq!("(if #t 1 (/ 'foo 'bar))", Ok(Integer(1)));
        assert_eval_eq!("(if #f (/ 'foo 'bar) 2)", Ok(Integer(2)));
        assert_eval_eq!("(if (= 2 2.0) 1 2)", Ok(Integer(1)));
        assert_eval_eq!("(if (= #t #f) 1 2)", Ok(Integer(2)));

        // errors
        assert_eval_eq!("(if #t)", Err(Error::WrongArgNum));
        assert_eval_eq!("(if #f 1)", Err(Error::WrongArgNum));
    }

    #[test]
    fn condfn() {
        assert_eval_eq!("(cond)", Ok(Sexpr::Nil));
        assert_eval_eq!("(cond (#t))", Ok(Sexpr::True));
        assert_eval_eq!("(cond ((= 2 2)))", Ok(Sexpr::True));
        assert_eval_eq!("(cond (#f 1) (#t 2))", Ok(Sexpr::Integer(2)));
        assert_eval_eq!(
            "(cond (#f (error 'oh 'no)) (#t (->integer (/ 4 2))))",
            Ok(Sexpr::Integer(2))
        );
        assert_eval_eq!(
            "(cond (#f (error 'first 'err)) (#t 'ok) (#t (error 'second 'err)))",
            Ok(Sexpr::symbol("ok"))
        );

        // errors
        assert_eval_eq!("(cond ())", Err(Error::WrongArgNum));
        assert_eval_eq!("(cond #t)", Err(Error::WrongArg(Sexpr::True)));
    }

    #[test]
    fn define() {
        let mut env = root_env();

        assert!(parse_eval!("(define x 'foo)", &mut env).is_ok());
        assert_eq!(env.get(&String::from("x")), Some(Sexpr::symbol("foo")));

        assert!(parse_eval!("(define x 'bar)", &mut env).is_ok());
        assert_eq!(env.get(&String::from("x")), Some(Sexpr::symbol("bar")));

        assert!(parse_eval!("(define y x)", &mut env).is_ok());
        assert_eq!(env.get(&String::from("y")), Some(Sexpr::symbol("bar")));

        assert!(parse_eval!("(define y (+ 2 (/ 4 2)))", &mut env).is_ok());
        assert_eq!(env.get(&String::from("y")), Some(Sexpr::Float(4.0)));

        assert_eq!(env.get(&String::from("x")), Some(Sexpr::symbol("bar")));

        // errors
        assert_eq!(
            parse_eval!("(define \"x\" 'foo)", &mut env),
            Err(Error::WrongArg(Sexpr::from("x")))
        );
        assert_eq!(parse_eval!("(define x)", &mut env), Err(Error::WrongArgNum));
        assert_eq!(
            parse_eval!("(define x 'foo 'bar)", &mut env),
            Err(Error::WrongArgNum)
        );
    }

    #[test]
    fn lambda() {
        assert_eval_eq!("((lambda (x) x) 42)", Ok(Sexpr::Integer(42)));
        assert_eval_eq!("((lambda (x y) (+ x y)) 5 6)", Ok(Sexpr::Integer(11)));
        assert_eval_eq!(
            "((lambda (x) ((lambda (y) (+ x y)) x)) 2)",
            Ok(Sexpr::Integer(4))
        );

        // errors
        assert_eval_eq!("(lambda (5) x)", Err(Error::WrongArg(Sexpr::Integer(5))));
        assert_eval_eq!("(lambda (x #t y) x)", Err(Error::WrongArg(Sexpr::True)));
        assert_eval_eq!(
            "(lambda (\"x\") x)",
            Err(Error::WrongArg(Sexpr::String(String::from("x"))))
        );
        assert_eval_eq!("((lambda (x y) (+ x y)) 1)", Err(Error::WrongArgNum));
        assert_eval_eq!("((lambda (x) x) 1 2)", Err(Error::WrongArgNum));
    }

    #[test]
    fn let_core() {
        assert_eval_eq!("(let () 'ok)", Ok(Sexpr::symbol("ok")));
        assert_eval_eq!("(let ((x 1) (y 2)) (+ x y))", Ok(Sexpr::Integer(3)));
        assert_eval_eq!("(let ((x 1) (y 2)) (+ x y) (* y y))", Ok(Sexpr::Integer(4)));
        assert_eval_eq!("(let ((x 1)) (let ((y x)) (+ x y)))", Ok(Sexpr::Integer(2)));

        // errors
        assert_eval_eq!(
            "(let 'wat '())",
            Err(Error::WrongArg(Sexpr::quote(Sexpr::symbol("wat"))))
        );
        assert_eval_eq!(
            "(let ((x 1) y 2) (+ x y))",
            Err(Error::WrongArg(Sexpr::symbol("y")))
        );

        // it's not let*
        assert_eval_eq!(
            "(let ((x 1) (y (+ x 1))) (+ x y))",
            Err(Error::NotFound(String::from("x")))
        );
    }

    #[test]
    fn closures() {
        assert_eval_eq!(
            "(((lambda (x) (lambda (y) (/ x y))) 1) 2)",
            Ok(Sexpr::Float(0.5))
        );
        assert_eval_eq!(
            "((lambda (x) (let ((x (+ x 1))) (* x 2))) 1)",
            Ok(Sexpr::Integer(4))
        );
        assert_eval_eq!(
            "(let ((x 2)) ((lambda (y) (+ y (+ x 1))) x))",
            Ok(Sexpr::Integer(5))
        );
        assert_eval_eq!(
            "((let ((x 2)) (lambda (y) (+ x y))) 3)",
            Ok(Sexpr::Integer(5))
        );
        assert_eval_eq!(
            "((let* ((x 1) (z (+ x 1))) (lambda (y) (+ x y z))) 3)",
            Ok(Sexpr::Integer(6))
        );
    }

    #[test]
    fn let_star() {
        // Works like let
        assert_eval_eq!("(let* () 'ok)", Ok(Sexpr::symbol("ok")));
        assert_eval_eq!("(let* ((x 1) (y 2)) (+ x y))", Ok(Sexpr::Integer(3)));
        assert_eval_eq!(
            "(let* ((x 1) (y 2)) (+ x y) (* y y))",
            Ok(Sexpr::Integer(4))
        );
        assert_eval_eq!(
            "(let* ((x 1)) (let* ((y x)) (+ x y)))",
            Ok(Sexpr::Integer(2))
        );
        assert_eval_eq!(
            "(let* ((x 1) (y (- 3 x))) (car '(1 2 3)) (+ x y))",
            Ok(Sexpr::Integer(3))
        );

        // let* recursive evaluation
        assert_eval_eq!("(let* ((x 1) (y (+ x 1))) (+ x y))", Ok(Sexpr::Integer(3)));
    }

    #[test]
    fn logic() {
        assert_eval_eq!("(not #f)", Ok(Sexpr::True));
        assert_eval_eq!("(not)", Ok(Sexpr::False));
        assert_eval_eq!("(not #t)", Ok(Sexpr::False));
        assert_eval_eq!("(not '())", Ok(Sexpr::False));
        assert_eval_eq!("(not 0)", Ok(Sexpr::False));

        assert_eval_eq!("(and)", Ok(Sexpr::True));
        assert_eval_eq!("(and #t)", Ok(Sexpr::True));
        assert_eval_eq!("(and #t '() 42 (= 2 (+ 1 1)))", Ok(Sexpr::True));
        assert_eval_eq!("(and #t '() #f 42)", Ok(Sexpr::False));

        assert_eval_eq!("(or)", Ok(Sexpr::False));
        assert_eval_eq!("(or #f)", Ok(Sexpr::False));
        assert_eval_eq!("(or #t)", Ok(Sexpr::True));
        assert_eval_eq!("(or #f (= 5 6) #t #f)", Ok(Sexpr::True));
    }

    #[test]
    fn type_checkers() {
        assert_eval_eq!("(bool? #t)", Ok(Sexpr::True));
        assert_eval_eq!("(bool? #f)", Ok(Sexpr::True));
        assert_eval_eq!("(bool? (= 2 2))", Ok(Sexpr::True));
        assert_eval_eq!("(bool? 0)", Ok(Sexpr::False));
        assert_eval_eq!("(bool? 'true)", Ok(Sexpr::False));
        assert_eval_eq!("(bool? '())", Ok(Sexpr::False));

        assert_eval_eq!("(symbol? 'foo)", Ok(Sexpr::True));
        assert_eval_eq!("(symbol? \"foo\")", Ok(Sexpr::False));

        assert_eval_eq!("(string? \"hello world!\")", Ok(Sexpr::True));
        assert_eval_eq!("(string? 'hello)", Ok(Sexpr::False));

        assert_eval_eq!("(number? -5)", Ok(Sexpr::True));
        assert_eval_eq!("(number? 3.14)", Ok(Sexpr::True));
        assert_eval_eq!("(number? (/ 5 (- 100 (* 7 2))))", Ok(Sexpr::True));
        assert_eval_eq!("(number? \"42\")", Ok(Sexpr::False));
        assert_eval_eq!("(number? #t)", Ok(Sexpr::False));
        assert_eval_eq!("(number? '())", Ok(Sexpr::False));

        assert_eval_eq!("(integer? 42)", Ok(Sexpr::True));
        assert_eval_eq!("(integer? 3.14)", Ok(Sexpr::False));
        assert_eval_eq!("(integer? 'foo)", Ok(Sexpr::False));

        assert_eval_eq!("(float? 3.14)", Ok(Sexpr::True));
        assert_eval_eq!("(float? 42)", Ok(Sexpr::False));
        assert_eval_eq!("(float? 'foo)", Ok(Sexpr::False));

        assert_eval_eq!("(null? '())", Ok(Sexpr::True));
        assert_eval_eq!("(null? (list))", Ok(Sexpr::True));
        assert_eval_eq!("(null? (cdr '(1)))", Ok(Sexpr::True));
        assert_eval_eq!("(null? (list 1 2 3))", Ok(Sexpr::False));
        assert_eval_eq!("(null? '(1 2 3))", Ok(Sexpr::False));
        assert_eval_eq!("(null? '(()))", Ok(Sexpr::False));
        assert_eval_eq!("(null? #t)", Ok(Sexpr::False));

        assert_eval_eq!("(pair? (list 1 2 3))", Ok(Sexpr::True));
        assert_eval_eq!("(pair? '(1))", Ok(Sexpr::True));
        assert_eval_eq!("(pair? '())", Ok(Sexpr::False));
        assert_eval_eq!("(pair? 'foo)", Ok(Sexpr::False));

        assert_eval_eq!("(procedure? car)", Ok(Sexpr::True));
        assert_eval_eq!("(procedure? (lambda (x) x))", Ok(Sexpr::True));
        assert_eval_eq!("(procedure? 'car)", Ok(Sexpr::False));
    }

    #[test]
    fn conversions() {
        assert_eval_eq!("(->integer 42)", Ok(Sexpr::Integer(42)));
        assert_eval_eq!("(->integer 3.14)", Ok(Sexpr::Integer(3)));
        assert_eval_eq!("(->integer (/ 10 3))", Ok(Sexpr::Integer(3)));
        assert_eval_eq!("(->integer \"-53\")", Ok(Sexpr::Integer(-53)));
        assert_eval_eq!(
            "(->integer 'foo)",
            Err(Error::CannotParse(Sexpr::symbol("foo")))
        );

        assert_eval_eq!("(->float 3.14)", Ok(Sexpr::Float(3.14)));
        assert_eval_eq!("(->float 5)", Ok(Sexpr::Float(5.0)));
        assert_eval_eq!("(->float (+ 10 1))", Ok(Sexpr::Float(11.0)));
        assert_eval_eq!("(->float \"365\")", Ok(Sexpr::Float(365.0)));
        assert_eval_eq!("(->float '())", Err(Error::CannotParse(Sexpr::null())));
    }

    #[test]
    fn set() {
        let root = &mut root_env();
        let mut local1 = root.branch();
        local1.insert(&String::from("foo"), &Sexpr::Integer(42));
        let mut local2 = local1.branch();

        assert_eq!(local2.get(&String::from("foo")), Some(Sexpr::Integer(42)));
        assert_eq!(
            parse_eval!("(set! foo (car (list 'bar)))", &mut local2),
            Ok(Sexpr::symbol("bar"))
        );

        assert_eq!(local2.get(&String::from("foo")), Some(Sexpr::symbol("bar")));
        assert_eq!(local1.get(&String::from("foo")), Some(Sexpr::symbol("bar")));
        assert_eq!(root.get(&String::from("foo")), None);

        assert_eq!(
            parse_eval!("(set! pi 3.14)", &mut local2),
            Err(Error::NotFound(String::from("pi")))
        );
    }

    #[test]
    fn evalfn() {
        assert_eval_eq!("(eval ''foo)", Ok(Sexpr::symbol("foo")));
        assert_eval_eq!("(eval '(+ 2 2))", Ok(Sexpr::Integer(4)));
    }

    #[test]
    fn to_string() {
        assert_eval_eq!("(string)", Ok(Sexpr::from("")));
        assert_eval_eq!(
            "(string 1 #t '() 'foo (+ 2 2) \"hello!\")",
            Ok(Sexpr::String(String::from("1 #t () foo 4 \"hello!\"")))
        );
    }

    #[test]
    fn to_error() {
        assert_eval_eq!("(error)", Err(Error::Custom(String::from(""))));
        assert_eval_eq!(
            "(error 'hello 'world)",
            Err(Error::Custom(String::from("hello world")))
        );
    }

    #[test]
    fn begin() {
        let root = &mut root_env();

        assert_eq!(parse_eval!("(begin)", root), Ok(Sexpr::Nil));

        assert_eq!(
            parse_eval!("(begin (define foo 'bar) (+ 5 6) (+ 2 2))", root),
            Ok(Sexpr::Integer(4))
        );
        assert_eq!(root.get(&String::from("foo")), Some(Sexpr::symbol("bar")));

        // break evaluation on error
        assert_eq!(
            parse_eval!(
                "(begin (/ 70 5) (error 'expected) (define dont #f) (+ 2 2))",
                root
            ),
            Err(Error::Custom(String::from("expected")))
        );
        assert_eq!(root.get(&String::from("dont")), None);
    }

    #[test]
    fn fibo_vanilla() {
        let env = &mut root_env();

        assert!(parse_eval!(
            "(define fibo (lambda (n)
                (if (= n 0) 0
                    (if (= n 1) 1
                        (+ (fibo (- n 1))
                        (fibo (- n 2)))))))",
            env
        )
        .is_ok());

        assert_eq!(parse_eval!("(fibo 0)", env), Ok(Sexpr::Integer(0)));
        assert_eq!(parse_eval!("(fibo 1)", env), Ok(Sexpr::Integer(1)));
        assert_eq!(parse_eval!("(fibo 2)", env), Ok(Sexpr::Integer(1)));
        assert_eq!(parse_eval!("(fibo 3)", env), Ok(Sexpr::Integer(2)));
        assert_eq!(parse_eval!("(fibo 7)", env), Ok(Sexpr::Integer(13)));
        assert_eq!(parse_eval!("(fibo 9)", env), Ok(Sexpr::Integer(34)));
        assert_eq!(parse_eval!("(fibo 10)", env), Ok(Sexpr::Integer(55)));
    }

    #[test]
    fn fibo_tail_recur() {
        let env = &mut root_env();

        assert!(parse_eval!(
            "(define impl (lambda (it second first)
                (if (= it 0) first
                    (impl (- it 1) (+ first second) second))))",
            env
        )
        .is_ok());
        assert!(parse_eval!("(define fibo (lambda (n) (impl n 1.0 0.0)))", env).is_ok());

        assert_eq!(parse_eval!("(fibo 0)", env), Ok(Sexpr::Integer(0)));
        assert_eq!(parse_eval!("(fibo 1)", env), Ok(Sexpr::Integer(1)));
        assert_eq!(parse_eval!("(fibo 2)", env), Ok(Sexpr::Integer(1)));
        assert_eq!(parse_eval!("(fibo 3)", env), Ok(Sexpr::Integer(2)));
        assert_eq!(parse_eval!("(fibo 7)", env), Ok(Sexpr::Integer(13)));
        assert_eq!(parse_eval!("(fibo 9)", env), Ok(Sexpr::Integer(34)));
        assert_eq!(parse_eval!("(fibo 10)", env), Ok(Sexpr::Integer(55)));

        // this would fail without tail-call optimization
        let _ = parse_eval!("(fibo 1000)", env);
    }

    #[test]
    fn load() {
        let env = &mut root_env();
        assert_eq!(
            parse_eval!("(load \"examples/simple.scm\")", env),
            Ok(Sexpr::Integer(321))
        );
        assert_eq!(env.get(&String::from("x")), Some(Sexpr::Integer(1)));
    }
}
