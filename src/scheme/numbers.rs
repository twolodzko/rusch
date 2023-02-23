use std::cmp::Ordering;
use std::ops;

use super::utils::*;
use crate::errors::Error;
use crate::types::{Args, Env, Flt, FuncResult, Int, Sexpr};

pub fn add(args: &Args, env: &mut Env) -> FuncResult {
    list_reduce(args, env, Sexpr::Integer(0), |x, y| x + y)
}

pub fn sub(args: &Args, env: &mut Env) -> FuncResult {
    list_reduce(args, env, Sexpr::Integer(0), |x, y| x - y)
}

pub fn mul(args: &Args, env: &mut Env) -> FuncResult {
    list_reduce(args, env, Sexpr::Integer(1), |x, y| x * y)
}

pub fn div(args: &Args, env: &mut Env) -> FuncResult {
    if args.is_empty() {
        return Err(Error::WrongArgNum);
    }
    list_reduce(args, env, Sexpr::Integer(1), |x, y| x / y)
}

pub fn rem(args: &Args, env: &mut Env) -> FuncResult {
    if !args.has_next() {
        return Err(Error::WrongArgNum);
    }
    eval_iter(args, env)
        .reduce(|acc, elem| acc.and_then(|a| elem.and_then(|e| a % e)))
        .unwrap_or(Err(Error::WrongArgNum))
}

pub fn div_euclid(args: &Args, env: &mut Env) -> FuncResult {
    if args.is_empty() {
        return Err(Error::WrongArgNum);
    }
    list_reduce(args, env, Sexpr::Integer(1), |x, y| x.div_euclid(y))
}

pub fn gt(args: &Args, env: &mut Env) -> FuncResult {
    cmp(args, env, std::cmp::Ordering::Greater)
}

pub fn lt(args: &Args, env: &mut Env) -> FuncResult {
    cmp(args, env, std::cmp::Ordering::Less)
}

pub fn eq(args: &Args, env: &mut Env) -> FuncResult {
    cmp(args, env, std::cmp::Ordering::Equal)
}

pub fn to_integer(args: &Args, env: &mut Env) -> FuncResult {
    let sexpr = eval_one_arg(args, env)?;
    match &sexpr {
        Sexpr::Integer(_) => Ok(sexpr),
        Sexpr::Float(number) => Ok(Sexpr::Integer(*number as Int)),
        Sexpr::String(string) => match string.parse::<Int>() {
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
        Sexpr::Integer(number) => Ok(Sexpr::Float(*number as Flt)),
        Sexpr::String(string) => match string.parse::<Flt>() {
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

/// Fold the list
/// - when `args` is empty, return `init`
/// - when there is single element in `args`, return `func(init, arg)`
/// - for more elements, fold the list with `func(acc, arg)`
#[inline]
fn list_reduce(
    args: &Args,
    env: &mut Env,
    init: Sexpr,
    func: impl Fn(Sexpr, Sexpr) -> FuncResult,
) -> FuncResult {
    let iter = &mut eval_iter(args, env);
    let acc = match iter.next() {
        Some(sexpr) => sexpr?,
        None => return Ok(init),
    };
    if !args.has_next() {
        return func(init, acc);
    }
    iter.try_fold(acc, |acc, x| func(acc, x?))
}

/// Use `partial_cmp` to compare subsequent values
#[inline]
fn cmp(args: &Args, env: &mut Env, order: std::cmp::Ordering) -> FuncResult {
    let iter = &mut eval_iter(args, env);

    let mut prev = match iter.next() {
        Some(sexpr) => sexpr?,
        None => return Ok(Sexpr::True),
    };

    for elem in &mut *iter {
        let elem = elem?;
        let cmp =
            prev.partial_cmp(&elem.clone())
                .ok_or_else(|| match (prev.clone(), elem.clone()) {
                    (prev, Sexpr::Float(_) | Sexpr::Integer(_)) => Error::NotANumber(prev),
                    (_, elem) => Error::NotANumber(elem),
                })?;
        if cmp != order {
            return Ok(Sexpr::False);
        }
        prev = elem
    }
    Ok(Sexpr::True)
}

macro_rules! op {
    ( $lhs:tt $op:tt $rhs:tt ) => {
        {
            use Sexpr::{Float, Integer};
            match ($lhs, $rhs) {
                (Integer(x), Integer(y)) => Ok(Integer(x $op y)),
                (Integer(x), Float(y)) => Ok(Float(x as Flt $op y)),
                (Float(x), Integer(y)) => Ok(Float(x $op y as Flt)),
                (Float(x), Float(y)) => Ok(Float(x $op y)),
                (Float(_) | Integer(_), y) => Err(Error::NotANumber(y)),
                (x, _) => Err(Error::NotANumber(x)),
            }
        }
    };
}

impl ops::Add<Sexpr> for Sexpr {
    type Output = FuncResult;

    fn add(self, rhs: Self) -> Self::Output {
        op!(self + rhs)
    }
}

impl ops::Sub<Sexpr> for Sexpr {
    type Output = FuncResult;

    fn sub(self, rhs: Self) -> Self::Output {
        op!(self - rhs)
    }
}

impl ops::Mul<Sexpr> for Sexpr {
    type Output = FuncResult;

    fn mul(self, rhs: Self) -> Self::Output {
        op!(self * rhs)
    }
}

impl ops::Div<Sexpr> for Sexpr {
    type Output = FuncResult;

    fn div(self, rhs: Self) -> Self::Output {
        use Sexpr::{Float, Integer};
        match (self, rhs) {
            (Integer(x), Integer(y)) => (y as Flt).non_zero().map(|y| Float(x as Flt / y)),
            (Integer(x), Float(y)) => y.non_zero().map(|y| Float(x as Flt / y)),
            (Float(x), Integer(y)) => (y as Flt).non_zero().map(|y| Float(x / y)),
            (Float(x), Float(y)) => y.non_zero().map(|y| Float(x / y)),
            (Float(_) | Integer(_), y) => Err(Error::NotANumber(y)),
            (x, _) => Err(Error::NotANumber(x)),
        }
    }
}

macro_rules! non_zero_op {
    ( $func:tt $lhs:tt $rhs:tt ) => {{
        use Sexpr::{Float, Integer};
        match ($lhs, $rhs) {
            (Integer(x), Integer(y)) => y.non_zero().map(|y| Integer(x.$func(y))),
            (Integer(x), Float(y)) => y.non_zero().map(|y| Float((x as Flt).$func(y))),
            (Float(x), Integer(y)) => (y as Flt).non_zero().map(|y| Float(x.$func(y))),
            (Float(x), Float(y)) => y.non_zero().map(|y| Float(x.$func(y))),
            (Float(_) | Integer(_), y) => Err(Error::NotANumber(y)),
            (x, _) => Err(Error::NotANumber(x)),
        }
    }};
}

impl Sexpr {
    #[inline]
    fn div_euclid(self, rhs: Self) -> FuncResult {
        non_zero_op!(div_euclid self rhs)
    }
}

impl ops::Rem<Sexpr> for Sexpr {
    type Output = FuncResult;

    fn rem(self, rhs: Self) -> Self::Output {
        non_zero_op!(rem self rhs)
    }
}

impl std::cmp::PartialOrd for Sexpr {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        match (self, other) {
            (Sexpr::Integer(x), Sexpr::Integer(y)) => x.partial_cmp(y),
            (Sexpr::Integer(x), Sexpr::Float(y)) => (*x as Flt).partial_cmp(y),
            (Sexpr::Float(x), Sexpr::Integer(y)) => x.partial_cmp(&(*y as Flt)),
            (Sexpr::Float(x), Sexpr::Float(y)) => x.partial_cmp(y),
            _ => None,
        }
    }
}

trait NonZero<T> {
    fn non_zero(self) -> Result<T, Error<Sexpr>>;
}

impl NonZero<Int> for Int {
    #[inline]
    fn non_zero(self) -> Result<Int, Error<Sexpr>> {
        if self == 0 {
            return Err(Error::Undefined);
        }
        Ok(self)
    }
}

impl NonZero<Flt> for Flt {
    #[inline]
    fn non_zero(self) -> Result<Flt, Error<Sexpr>> {
        if self == 0.0 {
            return Err(Error::Undefined);
        }
        Ok(self)
    }
}

#[cfg(test)]
mod tests {
    use super::Sexpr;

    #[test]
    fn math() {
        use crate::errors::Error;
        use Sexpr::{Float, Integer};

        assert_eq!(Integer(2) + Integer(2), Ok(Integer(4)));
        assert_eq!(Integer(2) + Float(2.0), Ok(Float(4.0)));
        assert_eq!(Float(2.0) + Integer(2), Ok(Float(4.0)));
        assert_eq!(Float(2.0) + Float(2.0), Ok(Float(4.0)));
        assert_eq!(
            Float(2.0) + Sexpr::True,
            Err(Error::NotANumber(Sexpr::True))
        );
        assert_eq!(
            Sexpr::True + Float(2.0),
            Err(Error::NotANumber(Sexpr::True))
        );

        assert_eq!(Integer(2) - Integer(1), Ok(Integer(1)));
        assert_eq!(Integer(2) - Float(1.0), Ok(Float(1.0)));
        assert_eq!(Float(2.0) - Integer(1), Ok(Float(1.0)));
        assert_eq!(Float(2.0) - Float(1.0), Ok(Float(1.0)));
        assert_eq!(
            Float(2.0) - Sexpr::True,
            Err(Error::NotANumber(Sexpr::True))
        );
        assert_eq!(
            Sexpr::True - Float(2.0),
            Err(Error::NotANumber(Sexpr::True))
        );

        assert_eq!(Integer(2) * Integer(2), Ok(Integer(4)));
        assert_eq!(Integer(2) * Float(2.0), Ok(Float(4.0)));
        assert_eq!(Float(2.0) * Integer(2), Ok(Float(4.0)));
        assert_eq!(Float(2.0) * Float(2.0), Ok(Float(4.0)));
        assert_eq!(
            Float(2.0) * Sexpr::True,
            Err(Error::NotANumber(Sexpr::True))
        );
        assert_eq!(
            Sexpr::True * Float(2.0),
            Err(Error::NotANumber(Sexpr::True))
        );

        assert_eq!(Integer(4) / Integer(2), Ok(Float(2.0)));
        assert_eq!(Integer(4) / Float(2.0), Ok(Float(2.0)));
        assert_eq!(Float(4.0) / Integer(2), Ok(Float(2.0)));
        assert_eq!(Float(4.0) / Float(2.0), Ok(Float(2.0)));
        assert_eq!(
            Float(4.0) / Sexpr::True,
            Err(Error::NotANumber(Sexpr::True))
        );
        assert_eq!(
            Sexpr::True / Float(4.0),
            Err(Error::NotANumber(Sexpr::True))
        );

        assert_eq!(Integer(5) / Integer(0), Err(Error::Undefined));
        assert_eq!(Integer(5) / Float(0.0), Err(Error::Undefined));
        assert_eq!(Float(5.0) / Integer(0), Err(Error::Undefined));
        assert_eq!(Float(5.0) / Float(0.0), Err(Error::Undefined));

        assert_eq!(Integer(5) % Integer(2), Ok(Integer(1)));
        assert_eq!(Integer(5) % Float(2.0), Ok(Float(1.0)));
        assert_eq!(Float(5.0) % Integer(2), Ok(Float(1.0)));
        assert_eq!(Float(5.0) % Float(2.0), Ok(Float(1.0)));
        assert_eq!(Integer(5) % Integer(0), Err(Error::Undefined));
        assert_eq!(Integer(5) % Float(0.0), Err(Error::Undefined));
        assert_eq!(Float(5.0) % Integer(0), Err(Error::Undefined));
        assert_eq!(Float(5.0) % Float(0.0), Err(Error::Undefined));
        assert_eq!(
            Float(5.0) % Sexpr::True,
            Err(Error::NotANumber(Sexpr::True))
        );
        assert_eq!(
            Sexpr::True % Float(5.0),
            Err(Error::NotANumber(Sexpr::True))
        );
    }

    #[test]
    fn comparisons() {
        use Sexpr::*;

        assert!(Integer(2) > Integer(1));
        assert!(Integer(2) >= Integer(1));
        assert!(Float(2.0) > Integer(1));
        assert!(Integer(2) > Float(1.0));
        assert!(Integer(1) < Integer(2));
        assert!(Integer(100) <= Integer(200));

        assert_eq!(Sexpr::True.partial_cmp(&Float(5.0)), None);
        assert_eq!(Float(5.0).partial_cmp(&Sexpr::True), None);
        assert_eq!(Sexpr::True.partial_cmp(&Integer(1)), None);
        assert_eq!(Integer(1).partial_cmp(&Sexpr::True), None);
    }
}
