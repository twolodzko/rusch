use crate::envir;
use crate::errors::Error;
use crate::eval::{eval, eval_but_last, eval_file};
use crate::list::List;
use crate::types::{FuncResult, Sexpr, TcoResult};

type Args = List<Sexpr>;
type Env = envir::Env<Sexpr>;

pub fn quote(args: &Args, _env: &mut Env) -> FuncResult {
    if args.has_next() {
        return Err(Error::WrongArgNum);
    }
    head_or_err(args).cloned()
}

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

pub fn not(args: &Args, env: &mut Env) -> FuncResult {
    if args.is_empty() {
        return Ok(Sexpr::False);
    }
    let result = eval_one_arg(args, env)?;
    Ok(Sexpr::from(!result.is_true()))
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

#[inline]
fn cmp(args: &Args, env: &mut Env, order: std::cmp::Ordering) -> FuncResult {
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

pub fn gt(args: &Args, env: &mut Env) -> FuncResult {
    cmp(args, env, std::cmp::Ordering::Greater)
}

pub fn lt(args: &Args, env: &mut Env) -> FuncResult {
    cmp(args, env, std::cmp::Ordering::Less)
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
    fn from_symbol(key: &String, rhs: &List<Sexpr>, env: &mut Env) -> FuncResult {
        if rhs.has_next() {
            return Err(Error::WrongArgNum);
        }
        let sexpr = rhs.head().unwrap();
        let val = eval(sexpr, env)?;
        env.insert(key, val);
        Ok(Sexpr::Nil)
    }

    #[inline]
    fn from_list(list: &List<Sexpr>, rhs: &List<Sexpr>, env: &mut Env) -> FuncResult {
        let key = head_or_err(list).and_then(symbol_or_err)?;
        let vars = list.tail_or_empty();
        let lambda = lambda_init(&vars, rhs, env)?;
        env.insert(key, lambda);
        Ok(Sexpr::Nil)
    }

    let rhs = args.tail().ok_or(Error::WrongArgNum)?;
    match head_or_err(args)? {
        Sexpr::Symbol(key) => from_symbol(key, &rhs, env),
        Sexpr::List(list) => from_list(list, &rhs, env),
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

pub fn evalfn(args: &Args, env: &mut Env) -> FuncResult {
    if args.has_next() {
        return Err(Error::WrongArgNum);
    }
    let sexpr = head_or_err(args)?;
    eval(&eval(sexpr, env)?, env)
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

pub fn reverse(args: &Args, env: &mut Env) -> FuncResult {
    eval_one_arg(args, env)
        .and_then(|ref elem| list_or_err(elem).cloned())
        .map(|list| Sexpr::List(list.rev()))
}

// Utils

/// Collect list of S-expressions to a space-separated string
#[inline]
fn stringify(args: &Args, env: &mut Env) -> Result<String, Error<Sexpr>> {
    let string = eval_iter(args, env)
        .map(|elem| elem.map(|sexpr| sexpr.to_string()))
        .collect::<Result<Vec<String>, Error<Sexpr>>>()?;
    Ok(string.join(" "))
}

/// If `Sexpr` is a list, return enclosed list, otherwise throw an error
#[inline]
fn list_or_err(sexpr: &Sexpr) -> Result<&List<Sexpr>, Error<Sexpr>> {
    match sexpr {
        Sexpr::List(list) => Ok(list),
        sexpr => Err(Error::WrongArg(sexpr.clone())),
    }
}

/// If `Sexpr` is a symbol, return it's name, otherwise throw an error
#[inline]
fn symbol_or_err(sexpr: &Sexpr) -> Result<&String, Error<Sexpr>> {
    match sexpr {
        Sexpr::Symbol(name) => Ok(name),
        sexpr => Err(Error::WrongArg(sexpr.clone())),
    }
}

/// Extract head of a list, for empty list throw an error
#[inline]
fn head_or_err(args: &Args) -> Result<&Sexpr, Error<Sexpr>> {
    args.head().ok_or(Error::WrongArgNum)
}

/// Initialize iterator that evaluates and returns each element of the list
#[inline]
fn eval_iter<'a>(args: &'a List<Sexpr>, env: &'a mut Env) -> impl Iterator<Item = FuncResult> + 'a {
    args.iter().map(|elem| eval(elem, env))
}

/// Evaluate first argument, raise error if more arguments were passed
#[inline]
fn eval_one_arg(args: &Args, env: &mut Env) -> FuncResult {
    if args.has_next() {
        return Err(Error::WrongArgNum);
    }
    head_or_err(args).and_then(|sexpr| eval(sexpr, env))
}
