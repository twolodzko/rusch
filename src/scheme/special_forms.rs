use super::utils::*;
use crate::errors::Error;
use crate::eval::{eval, eval_but_last, eval_file};
use crate::list::List;
use crate::types::{Args, Env, FuncResult, Sexpr, TcoResult};

// See: https://groups.csail.mit.edu/mac/ftpdir/scheme-7.4/doc-html/scheme_3.html

pub fn quote(args: &Args, _env: &mut Env) -> FuncResult {
    one_arg_or_err(args).cloned()
}

pub fn quasiquote(args: &Args, env: &mut Env) -> FuncResult {
    one_arg_or_err(args).and_then(|sexpr| qq_eval(sexpr, env, 1))
}

/// Evaluate quasiquoted expression
fn qq_eval(sexpr: &Sexpr, env: &mut Env, quotes: i32) -> FuncResult {
    let quotes = &mut quotes.clone();

    let list = match sexpr {
        Sexpr::List(list) => list,
        _ => return Ok(sexpr.clone()),
    };
    if let Some(Sexpr::Symbol(name)) = list.head() {
        if name == &String::from("unquote") {
            *quotes -= 1
        } else if name == &String::from("quasiquote") {
            *quotes += 1
        }
    };
    if *quotes == 0 {
        return eval(sexpr, env);
    }
    list.iter()
        .map(|elem| qq_eval(elem, env, *quotes))
        .collect()
}

pub fn lambda(args: &Args, env: &mut Env) -> FuncResult {
    let vars = head_or_err(args).and_then(list_or_err)?;
    let body = args.tail_or_empty();
    lambda_init(vars, &body, env)
}

#[inline]
pub fn lambda_init(vars: &List<Sexpr>, body: &List<Sexpr>, env: &mut Env) -> FuncResult {
    let vars: Result<Vec<String>, Error<Sexpr>> = vars
        .iter()
        .map(|elem| symbol_or_err(elem).cloned())
        .collect();
    Ok(Sexpr::lambda(vars?, body.clone(), env.clone()))
}

pub fn iffn(args: &Args, env: &mut Env) -> TcoResult {
    let mut iter = args.iter();
    let sexpr = iter.next().ok_or(Error::WrongArgNum)?;
    if !eval(sexpr, env)?.is_true() {
        iter.next();
    }
    match iter.next() {
        Some(sexpr) => Ok((sexpr.clone(), Some(env.clone()))),
        None => Err(Error::WrongArgNum),
    }
}

pub fn condfn(args: &Args, env: &mut Env) -> TcoResult {
    for branch in args.iter() {
        let list = list_or_err(branch)?;
        let condition = eval(head_or_err(list)?, env)?;
        if condition.is_true() {
            match list.tail() {
                Some(ref body) => return eval_but_last(body, env),
                None => return Ok((condition, None)),
            }
        }
    }
    Ok((Sexpr::Nil, None))
}

pub fn define(args: &Args, env: &mut Env) -> FuncResult {
    #[inline]
    fn from_symbol(key: &String, rest: &List<Sexpr>, env: &mut Env) -> FuncResult {
        if rest.has_next() {
            return Err(Error::WrongArgNum);
        }
        let sexpr = rest.head().unwrap();
        let val = eval(sexpr, env)?;
        env.insert(key, val);
        Ok(Sexpr::Nil)
    }

    #[inline]
    fn from_list(list: &List<Sexpr>, rest: &List<Sexpr>, env: &mut Env) -> FuncResult {
        let key = head_or_err(list).and_then(symbol_or_err)?;
        let vars = list.tail_or_empty();
        let lambda = lambda_init(&vars, rest, env)?;
        env.insert(key, lambda);
        Ok(Sexpr::Nil)
    }

    let rest = args.tail().ok_or(Error::WrongArgNum)?;
    match head_or_err(args)? {
        Sexpr::Symbol(key) => from_symbol(key, &rest, env),
        Sexpr::List(list) => from_list(list, &rest, env),
        sexpr => Err(Error::WrongArg(sexpr.clone())),
    }
}

pub fn set(args: &Args, env: &mut Env) -> FuncResult {
    let mut iter = args.iter();

    let key = iter
        .next()
        .ok_or(Error::WrongArgNum)
        .and_then(symbol_or_err)?;
    let val = iter.next().ok_or(Error::WrongArgNum)?;

    if iter.next().is_some() {
        // has third arg
        return Err(Error::WrongArgNum);
    }

    match env.find_env(key) {
        Some(ref mut local) => {
            local.insert(key, eval(val, env)?);
            Ok(Sexpr::Nil)
        }
        None => Err(Error::NotFound(key.clone())),
    }
}

#[inline]
fn eval_and_bind(
    args: &Args,
    eval_env: &mut Env,
    save_env: &mut Env,
) -> Result<String, Error<Sexpr>> {
    let mut iter = args.iter();

    let key = iter
        .next()
        .ok_or(Error::WrongArgNum)
        .and_then(symbol_or_err)?;
    let val = iter.next().ok_or(Error::WrongArgNum)?;

    if iter.next().is_some() {
        // has third arg
        return Err(Error::WrongArgNum);
    }

    save_env.insert(key, eval(val, eval_env)?);
    Ok(key.clone())
}

pub fn let_core(args: &Args, env: &mut Env) -> TcoResult {
    let local = &mut env.branch();
    match head_or_err(args)? {
        Sexpr::List(ref bindings) => {
            let body = args.tail_or_empty();
            let_impl(bindings, &body, env, local)
        }
        Sexpr::Symbol(ref key) => {
            let tail = args.tail().ok_or(Error::WrongArgNum)?;
            let bindings = head_or_err(&tail).and_then(list_or_err)?;
            let body = tail.tail_or_empty();
            named_let(key, bindings, &body, local)
        }
        sexpr => Err(Error::WrongArg(sexpr.clone())),
    }
}

pub fn let_star(args: &Args, env: &mut Env) -> TcoResult {
    let local = &mut env.branch();
    let bindings = head_or_err(args).and_then(list_or_err)?;
    let body = args.tail_or_empty();
    let_impl(bindings, &body, &mut local.clone(), local)
}

#[inline]
fn let_impl(bindings: &Args, body: &Args, call_env: &mut Env, eval_env: &mut Env) -> TcoResult {
    bindings.iter().try_for_each(|elem| {
        let binding = list_or_err(elem)?;
        eval_and_bind(binding, call_env, eval_env)?;
        Ok(())
    })?;
    eval_but_last(body, eval_env)
}

#[inline]
fn named_let(key: &String, bindings: &Args, body: &Args, env: &mut Env) -> TcoResult {
    let local = &mut env.branch();

    let vars: Result<Vec<String>, Error<Sexpr>> = bindings
        .iter()
        .map(|elem| {
            let binding = list_or_err(elem)?;
            eval_and_bind(binding, env, local)
        })
        .collect();
    let lambda = Sexpr::lambda(vars?, body.clone(), env.clone());
    env.insert(key, lambda);

    eval_but_last(body, local)
}

pub fn evalfn(args: &Args, env: &mut Env) -> FuncResult {
    eval_one_arg(args, env).and_then(|ref sexpr| eval(sexpr, env))
}

pub fn load(args: &Args, env: &mut Env) -> FuncResult {
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

pub fn andfn(args: &Args, env: &mut Env) -> FuncResult {
    let iter = &mut eval_iter(args, env);
    let mut last = Sexpr::True;
    for elem in &mut *iter {
        let elem = elem?;
        if !elem.is_true() {
            return Ok(Sexpr::False);
        };
        last = elem;
    }
    Ok(last)
}

pub fn orfn(args: &Args, env: &mut Env) -> FuncResult {
    let iter = &mut eval_iter(args, env);
    for elem in &mut *iter {
        let elem = elem?;
        if elem.is_true() {
            return Ok(elem);
        }
    }
    Ok(Sexpr::False)
}
