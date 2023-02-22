use super::utils::*;
use crate::errors::Error;
use crate::list::List;
use crate::types::{Args, Env, FuncResult, Sexpr};

pub fn car(args: &Args, env: &mut Env) -> FuncResult {
    let list = eval_one_arg(args, env).and_then(|ref arg| list_or_err(arg).cloned())?;
    list.head()
        .cloned()
        .ok_or_else(|| Error::WrongArg(Sexpr::List(list)))
}

pub fn cdr(args: &Args, env: &mut Env) -> FuncResult {
    let list = eval_one_arg(args, env).and_then(|ref arg| list_or_err(arg).cloned())?;
    if list.is_empty() {
        return Err(Error::WrongArg(Sexpr::List(list)));
    };
    Ok(Sexpr::List(list.tail_or_empty()))
}

pub fn cons(args: &Args, env: &mut Env) -> FuncResult {
    // (cons lhs rhs)
    let iter = &mut eval_iter(args, env);
    let lhs = iter.next().unwrap_or(Err(Error::WrongArgNum))?;
    let rhs = iter.next().unwrap_or(Err(Error::WrongArgNum))?;

    if iter.next().is_some() {
        return Err(Error::WrongArgNum);
    }

    let list = match rhs {
        Sexpr::List(list) => list,
        sexpr => List::from(sexpr),
    };
    Ok(Sexpr::List(list.push_front(lhs)))
}

pub fn list(args: &Args, env: &mut Env) -> FuncResult {
    eval_iter(args, env).collect()
}

pub fn not(args: &Args, env: &mut Env) -> FuncResult {
    if args.is_empty() {
        return Ok(Sexpr::False);
    }
    let result = eval_one_arg(args, env)?;
    Ok(Sexpr::from(!result.is_true()))
}

macro_rules! math_op {
    ( $op:tt, $lhs:expr, $rhs:expr ) => {
        {
            use Sexpr::{Float, Integer};
            match ($lhs, $rhs) {
                (Integer(x), Integer(y)) => Ok(Integer(x $op y)),
                (Integer(x), Float(y)) => Ok(Float(x as f64 $op y)),
                (Float(x), Integer(y)) => Ok(Float(x $op y as f64)),
                (Float(x), Float(y)) => Ok(Float(x $op y)),
                (Float(_) | Integer(_), y) => Err(Error::NotANumber(y)),
                (x, _) => Err(Error::NotANumber(x)),
            }
        }
    };
}

pub fn add(args: &Args, env: &mut Env) -> FuncResult {
    list_reduce(args, env, Sexpr::Integer(0), |x, y| math_op!(+, x, y))
}

pub fn sub(args: &Args, env: &mut Env) -> FuncResult {
    list_reduce(args, env, Sexpr::Integer(0), |x, y| math_op!(-, x, y))
}

pub fn mul(args: &Args, env: &mut Env) -> FuncResult {
    list_reduce(args, env, Sexpr::Integer(1), |x, y| math_op!(*, x, y))
}

pub fn div(args: &Args, env: &mut Env) -> FuncResult {
    #[inline]
    fn func(lhs: Sexpr, rhs: Sexpr) -> FuncResult {
        use Sexpr::{Float, Integer};
        match (lhs, rhs) {
            (Integer(x), Integer(y)) => (y as f64).non_zero().map(|y| Float(x as f64 / y)),
            (Integer(x), Float(y)) => y.non_zero().map(|y| Float(x as f64 / y)),
            (Float(x), Integer(y)) => (y as f64).non_zero().map(|y| Float(x / y)),
            (Float(x), Float(y)) => y.non_zero().map(|y| Float(x / y)),
            (Float(_) | Integer(_), y) => Err(Error::NotANumber(y)),
            (x, _) => Err(Error::NotANumber(x)),
        }
    }

    if args.is_empty() {
        return Err(Error::WrongArgNum);
    }
    list_reduce(args, env, Sexpr::Integer(1), func)
}

pub fn rem(args: &Args, env: &mut Env) -> FuncResult {
    #[inline]
    fn func(lhs: Sexpr, rhs: Sexpr) -> FuncResult {
        use Sexpr::{Float, Integer};
        match (lhs, rhs) {
            (Integer(x), Integer(y)) => y.non_zero().map(|y| Integer(x % y)),
            (Integer(x), Float(y)) => y.non_zero().map(|y| Float(x as f64 % y)),
            (Float(x), Integer(y)) => (y as f64).non_zero().map(|y| Float(x % y)),
            (Float(x), Float(y)) => y.non_zero().map(|y| Float(x % y)),
            (Float(_) | Integer(_), y) => Err(Error::NotANumber(y)),
            (x, _) => Err(Error::NotANumber(x)),
        }
    }

    if !args.has_next() {
        return Err(Error::WrongArgNum);
    }
    eval_iter(args, env)
        .reduce(|acc, elem| acc.and_then(|x| elem.and_then(|y| func(x, y))))
        .unwrap_or(Err(Error::WrongArgNum))
}

pub fn int_div(args: &Args, env: &mut Env) -> FuncResult {
    #[inline]
    fn func(x: Sexpr, y: Sexpr) -> FuncResult {
        use Sexpr::{Float, Integer};
        match (x, y) {
            (Integer(x), Integer(y)) => y.non_zero().map(|y| Integer(x / y)),
            (Integer(x), Float(y)) => (y as i64).non_zero().map(|y| Integer(x / y)),
            (Float(x), Integer(y)) => y.non_zero().map(|y| Integer(x as i64 / y)),
            (Float(x), Float(y)) => (y as i64).non_zero().map(|y| Integer(x as i64 / y)),
            (Float(_) | Integer(_), y) => Err(Error::NotANumber(y)),
            (x, _) => Err(Error::NotANumber(x)),
        }
    }

    if args.is_empty() {
        return Err(Error::WrongArgNum);
    }
    list_reduce(args, env, Sexpr::Integer(1), func)
}

pub fn equal(args: &Args, env: &mut Env) -> FuncResult {
    let iter = &mut eval_iter(args, env);

    let mut prev = match iter.next() {
        Some(sexpr) => sexpr?,
        None => return Ok(Sexpr::True),
    };

    for elem in &mut *iter {
        let elem = elem?;
        if elem != prev {
            return Ok(Sexpr::False);
        }
        prev = elem;
    }
    Ok(Sexpr::True)
}

pub fn gt(args: &Args, env: &mut Env) -> FuncResult {
    cmp(args, env, std::cmp::Ordering::Greater)
}

pub fn lt(args: &Args, env: &mut Env) -> FuncResult {
    cmp(args, env, std::cmp::Ordering::Less)
}

pub fn is_symbol(args: &Args, env: &mut Env) -> FuncResult {
    Ok(Sexpr::from(matches!(
        eval_one_arg(args, env)?,
        Sexpr::Symbol(_)
    )))
}

pub fn is_bool(args: &Args, env: &mut Env) -> FuncResult {
    Ok(Sexpr::from(matches!(
        eval_one_arg(args, env)?,
        Sexpr::True | Sexpr::False
    )))
}

pub fn is_string(args: &Args, env: &mut Env) -> FuncResult {
    Ok(Sexpr::from(matches!(
        eval_one_arg(args, env)?,
        Sexpr::String(_)
    )))
}

pub fn is_number(args: &Args, env: &mut Env) -> FuncResult {
    Ok(Sexpr::from(matches!(
        eval_one_arg(args, env)?,
        Sexpr::Integer(_) | Sexpr::Float(_)
    )))
}

pub fn is_integer(args: &Args, env: &mut Env) -> FuncResult {
    Ok(Sexpr::from(matches!(
        eval_one_arg(args, env)?,
        Sexpr::Integer(_)
    )))
}

pub fn is_float(args: &Args, env: &mut Env) -> FuncResult {
    Ok(Sexpr::from(matches!(
        eval_one_arg(args, env)?,
        Sexpr::Float(_)
    )))
}

pub fn is_procedure(args: &Args, env: &mut Env) -> FuncResult {
    Ok(Sexpr::from(matches!(
        eval_one_arg(args, env)?,
        Sexpr::Func(_) | Sexpr::Lambda(_) | Sexpr::Tco(_)
    )))
}

pub fn is_pair(args: &Args, env: &mut Env) -> FuncResult {
    match eval_one_arg(args, env)? {
        Sexpr::List(ref list) => Ok(Sexpr::from(!list.is_empty())),
        _ => Ok(Sexpr::False),
    }
}

pub fn is_null(args: &Args, env: &mut Env) -> FuncResult {
    match eval_one_arg(args, env)? {
        Sexpr::List(ref list) => Ok(Sexpr::from(list.is_empty())),
        _ => Ok(Sexpr::False),
    }
}

pub fn to_integer(args: &Args, env: &mut Env) -> FuncResult {
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

pub fn to_float(args: &Args, env: &mut Env) -> FuncResult {
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

pub fn to_string(args: &Args, env: &mut Env) -> FuncResult {
    stringify(args, env).map(Sexpr::String)
}

pub fn to_error(args: &Args, env: &mut Env) -> FuncResult {
    Err(Error::Custom(stringify(args, env)?))
}

pub fn display(args: &Args, env: &mut Env) -> FuncResult {
    println!("{}", stringify(args, env)?);
    Ok(Sexpr::Nil)
}

pub fn reverse(args: &Args, env: &mut Env) -> FuncResult {
    eval_one_arg(args, env)
        .and_then(|ref elem| list_or_err(elem).cloned())
        .map(|list| Sexpr::List(list.rev()))
}
