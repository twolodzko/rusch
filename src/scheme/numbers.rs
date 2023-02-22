use super::utils::*;
use crate::errors::Error;
use crate::types::{Args, Env, FuncResult, Sexpr};

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

pub fn gt(args: &Args, env: &mut Env) -> FuncResult {
    cmp(args, env, std::cmp::Ordering::Greater)
}

pub fn lt(args: &Args, env: &mut Env) -> FuncResult {
    cmp(args, env, std::cmp::Ordering::Less)
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

trait NonZero<T> {
    fn non_zero(self) -> Result<T, Error<Sexpr>>;
}

impl NonZero<i64> for i64 {
    #[inline]
    fn non_zero(self) -> Result<i64, Error<Sexpr>> {
        if self == 0 {
            return Err(Error::Undefined);
        }
        Ok(self)
    }
}

impl NonZero<f64> for f64 {
    #[inline]
    fn non_zero(self) -> Result<f64, Error<Sexpr>> {
        if self == 0.0 {
            return Err(Error::Undefined);
        }
        Ok(self)
    }
}
