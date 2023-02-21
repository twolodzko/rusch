use crate::errors::Error;
use crate::eval::eval;
use crate::list::List;
use crate::types::{Args, Env, FuncResult, Sexpr};

/// Collect list of S-expressions to a space-separated string
#[inline]
pub fn stringify(args: &Args, env: &mut Env) -> Result<String, Error<Sexpr>> {
    let string = eval_iter(args, env)
        .map(|elem| elem.map(|sexpr| sexpr.to_string()))
        .collect::<Result<Vec<String>, Error<Sexpr>>>()?;
    Ok(string.join(" "))
}

/// If `Sexpr` is a list, return enclosed list, otherwise throw an error
#[inline]
pub fn list_or_err(sexpr: &Sexpr) -> Result<&List<Sexpr>, Error<Sexpr>> {
    match sexpr {
        Sexpr::List(list) => Ok(list),
        sexpr => Err(Error::WrongArg(sexpr.clone())),
    }
}

/// If `Sexpr` is a symbol, return it's name, otherwise throw an error
#[inline]
pub fn symbol_or_err(sexpr: &Sexpr) -> Result<&String, Error<Sexpr>> {
    match sexpr {
        Sexpr::Symbol(name) => Ok(name),
        sexpr => Err(Error::WrongArg(sexpr.clone())),
    }
}

/// Extract head of a list, for empty list throw an error
#[inline]
pub fn head_or_err(args: &Args) -> Result<&Sexpr, Error<Sexpr>> {
    args.head().ok_or(Error::WrongArgNum)
}

/// Initialize iterator that evaluates and returns each element of the list
#[inline]
pub fn eval_iter<'a>(
    args: &'a List<Sexpr>,
    env: &'a mut Env,
) -> impl Iterator<Item = FuncResult> + 'a {
    args.iter().map(|elem| eval(elem, env))
}

/// Evaluate first argument, raise error if more arguments were passed
#[inline]
pub fn eval_one_arg(args: &Args, env: &mut Env) -> FuncResult {
    if args.has_next() {
        return Err(Error::WrongArgNum);
    }
    head_or_err(args).and_then(|sexpr| eval(sexpr, env))
}

#[inline]
pub fn list_reduce(
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

#[inline]
pub fn cmp(args: &Args, env: &mut Env, order: std::cmp::Ordering) -> FuncResult {
    let iter = &mut eval_iter(args, env);

    let mut prev = match iter.next() {
        Some(sexpr) => sexpr?,
        None => return Ok(Sexpr::True),
    };

    for elem in &mut *iter {
        let elem = elem?;
        let cmp = prev
            .partial_cmp(&elem)
            .ok_or_else(|| Error::NotANumber(elem.clone()))?;
        if cmp != order {
            return Ok(Sexpr::False);
        }
        prev = elem
    }
    Ok(Sexpr::True)
}
