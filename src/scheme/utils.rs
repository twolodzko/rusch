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

/// If `sexpr` is a list, return enclosed list, otherwise throw an error
#[inline]
pub fn list_or_err(sexpr: &Sexpr) -> Result<&List<Sexpr>, Error<Sexpr>> {
    match sexpr {
        Sexpr::List(list) => Ok(list),
        sexpr => Err(Error::WrongArg(sexpr.clone())),
    }
}

/// If `sexpr` is a symbol, return it's name, otherwise throw an error
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

#[inline]
pub fn one_arg_or_err(args: &Args) -> Result<&Sexpr, Error<Sexpr>> {
    if args.has_next() {
        return Err(Error::WrongArgNum);
    }
    head_or_err(args)
}

/// Evaluate first argument, raise error if more arguments were passed
#[inline]
pub fn eval_one_arg(args: &Args, env: &mut Env) -> FuncResult {
    one_arg_or_err(args).and_then(|sexpr| eval(sexpr, env))
}

/// Initialize iterator that evaluates and returns each element of the list
#[inline]
pub fn eval_iter<'a>(
    args: &'a List<Sexpr>,
    env: &'a mut Env,
) -> impl Iterator<Item = FuncResult> + 'a {
    args.iter().map(|elem| eval(elem, env))
}
