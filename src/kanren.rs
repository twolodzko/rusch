use crate::{list::List, types::{Env, Sexpr, Vars}};
use std::iter::zip;

pub struct Run {
    pub target: String,
    pub body: Vec<Sexpr>,
    results: Vec<Sexpr>,
}

#[derive(Debug, Clone)]
pub struct Conde {
    pub branches: Vec<Sexpr>,
    pub last: usize,
}

fn unify(lhs: &Sexpr, rhs: &Sexpr, mut vars: Vars, env: &mut Env) -> Option<Vars> {
    use Sexpr::*;
    match (lhs, rhs) {
        (Free(lhs), rhs) => todo!(),
        (lhs, Free(rhs)) => todo!(),
        (List(lhs), List(rhs)) => {
            for (l, r) in zip(lhs.iter(), rhs.iter()) {
                vars = unify(l, r, vars, env)?;
            }
            Some(vars)
        }
        _ => if lhs == rhs {
            Some(vars)
        } else {
            None
        },
    }
}

pub(crate) fn reset(sexpr: &Sexpr) -> Sexpr{
    use Sexpr::*;
    match sexpr {
        Conde(ref conde) => {
            let mut conde = conde.clone();
            conde.last = 0;
            Conde(conde)
        },
        List(ref list) => {
            List(list_map(list, reset))
        },
        Lambda(lambda) => {
            let mut lambda = lambda.clone();
            lambda.body = list_map(&lambda.body, reset);
            Lambda(lambda)
        },
        _ => sexpr.clone(),
    }
}

fn list_map(list: &List<Sexpr>, fun: fn(&Sexpr) -> Sexpr) -> List<Sexpr> {
    let list: Vec<Sexpr> = list.iter().map(|x| fun(x)).collect();
    List::from(list)
}
