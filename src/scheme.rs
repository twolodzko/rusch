mod numbers;
mod procedures;
mod special_forms;
mod utils;

use crate::eval::eval_but_last;
use crate::scheme::numbers::*;
use crate::scheme::procedures::*;
use crate::scheme::special_forms::*;
use crate::scheme::utils::eval_one_arg;
use crate::types::{Env, Sexpr};

pub fn root_env() -> Env {
    use crate::types::Sexpr::{Func, Tco};
    Env::from([
        ("-", Func(sub)),
        ("->float", Func(to_float)),
        ("->integer", Func(to_integer)),
        ("*", Func(mul)),
        ("/", Func(div)),
        ("//", Func(div_euclid)),
        ("%", Func(rem)),
        ("+", Func(add)),
        ("<", Func(lt)),
        ("=", Func(eq)),
        (">", Func(gt)),
        ("and", Func(andfn)),
        ("begin", Tco(eval_but_last)),
        ("bool?", Func(is_bool)),
        ("car", Func(car)),
        ("cdr", Func(cdr)),
        ("cond", Tco(condfn)),
        ("cons", Func(cons)),
        ("define", Func(define)),
        ("display", Func(display)),
        ("else", Sexpr::True),
        ("eq?", Func(equal)),
        ("equal?", Func(equal)),
        ("error", Func(to_error)),
        ("eval", Func(evalfn)),
        ("finite?", Func(is_finite)),
        ("float?", Func(is_float)),
        ("if", Tco(iffn)),
        ("integer?", Func(is_integer)),
        ("lambda", Func(lambda)),
        ("let", Tco(let_core)),
        ("let*", Tco(let_star)),
        ("list", Func(list)),
        ("load", Func(load)),
        ("nan?", Func(is_nan)),
        ("not", Func(not)),
        ("null?", Func(is_null)),
        ("number?", Func(is_number)),
        ("or", Func(orfn)),
        ("pair?", Func(is_pair)),
        ("procedure?", Func(is_procedure)),
        ("quasiquote", Func(quasiquote)),
        ("quote", Func(quote)),
        ("reverse", Func(reverse)),
        ("set!", Func(set)),
        ("string?", Func(is_string)),
        ("string", Func(to_string)),
        ("symbol?", Func(is_symbol)),
        ("unquote", Func(eval_one_arg)),
    ])
}

#[cfg(test)]
mod tests;
