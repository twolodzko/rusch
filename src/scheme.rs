use crate::eval::eval_but_last;
use crate::scheme::procedures::*;
use crate::scheme::special_forms::*;
use crate::types::{Env, Sexpr};

mod procedures;
mod special_forms;
mod utils;

pub fn root_env() -> Env {
    use crate::types::Sexpr::{Func, Tco};
    Env::from([
        ("-", Func(sub)),
        ("->float", Func(to_float)),
        ("->integer", Func(to_integer)),
        ("*", Func(mul)),
        ("/", Func(div)),
        ("+", Func(add)),
        ("<", Func(lt)),
        ("=", Func(equal)),
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
        ("float?", Func(is_float)),
        ("if", Tco(iffn)),
        ("integer?", Func(is_integer)),
        ("lambda", Func(lambda)),
        ("let", Tco(let_core)),
        ("let*", Tco(let_star)),
        ("list", Func(list)),
        ("load", Func(load)),
        ("not", Func(not)),
        ("null?", Func(is_null)),
        ("number?", Func(is_number)),
        ("or", Func(orfn)),
        ("pair?", Func(is_pair)),
        ("procedure?", Func(is_procedure)),
        ("quote", Func(quote)),
        ("reverse", Func(reverse)),
        ("set!", Func(set)),
        ("string?", Func(is_string)),
        ("string", Func(to_string)),
        ("symbol?", Func(is_symbol)),
    ])
}

#[cfg(test)]
mod tests;
