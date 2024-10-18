use crate::types::{Env, Sexpr};
use std::iter::zip;

fn unify(lhs: &Sexpr, rhs: &Sexpr, env: &mut Env) -> bool {
    use Sexpr::*;
    match (lhs, rhs) {
        (Free(lhs), rhs) => todo!(),
        (lhs, Free(rhs)) => todo!(),
        (List(lhs), List(rhs)) => zip(lhs.iter(), rhs.iter()).all(|(l, r)| unify(l, r, env)),
        _ => lhs == rhs,
    }
}
