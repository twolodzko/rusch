use rusch::{
    envir::Env,
    errors::ReadError,
    eval::{eval, eval_file},
    parser::read_sexpr,
    reader::StdinReader,
    scheme::root_env,
    types::Sexpr,
};
use std::env;

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
    let args: Vec<String> = env::args().collect();
    let env = &mut root_env();

    if args.len() < 2 {
        println!("Press ^C to exit.\n");

        let reader = &mut StdinReader::new().unwrap();
        loop {
            match read_sexpr(reader) {
                Ok(ref sexpr) => eval_and_print(sexpr, env),
                Err(ReadError::Interrupted) => return,
                Err(msg) => println!("Error: {}", msg),
            }
        }
    }

    if &args[1] == "-h" || &args[1] == "--help" {
        println!(
            "Usage: {} [FILE]...\n\nIf no files are given, opens REPL.",
            args[0]
        );
        return;
    }

    for arg in args.iter().skip(1) {
        match eval_file(arg, env) {
            Ok(_) => (),
            Err(msg) => panic!("{}", msg),
        }
    }
}
