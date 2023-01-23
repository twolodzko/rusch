use std::env;

use rusch::envir::Env;
use rusch::eval::{eval, eval_file};
use rusch::io::StdinReader;
use rusch::parser::read_sexpr;
use rusch::scheme::root_env;
use rusch::types::Sexpr;

fn eval_and_print(sexpr: &Sexpr, env: &mut Env<Sexpr>) {
    match eval(sexpr, env) {
        Ok(result) => {
            if result != Sexpr::Nil {
                println!("{}", result)
            }
        }
        Err(msg) => println!("Error: {}", msg),
    }
}

fn main() {
    let args = &mut env::args().skip(1);

    let env = &mut root_env();

    if args.len() == 0 {
        println!("Press ^C to exit.\n");

        let reader = &mut StdinReader::new().unwrap();
        loop {
            match read_sexpr(reader) {
                Ok(ref sexpr) => eval_and_print(sexpr, env),
                Err(msg) => println!("Error: {}", msg),
            }
        }
    }

    for arg in args {
        match eval_file(&arg, env) {
            Ok(_) => (),
            Err(msg) => panic!("{}", msg),
        }
    }
}
