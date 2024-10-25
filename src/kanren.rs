#![expect(dead_code)]
#![expect(unused_variables)]

use crate::types::{Env, FuncResult, Sexpr, Vars};
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

#[expect(clippy::only_used_in_recursion)]
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
        _ => {
            if lhs == rhs {
                Some(vars)
            } else {
                None
            }
        }
    }
}

impl Sexpr {
    // Query the miniKanren expression
    pub(crate) fn query(&mut self, env: &mut Env) -> FuncResult {
        use Sexpr::*;
        match self {
            Conde(conde) => {
                // reset conde and run it
                conde.last = 0;
                todo!()
            }
            Lambda(lambda) => {
                // query lambda here
                todo!()
            }
            _ => Ok(Sexpr::False),
        }
    }

    // Move to the next miniKanren expression and query it
    pub(crate) fn query_next(&mut self, env: &mut Env) -> FuncResult {
        todo!()
    }
}
