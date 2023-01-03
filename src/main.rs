use std::env;

use rusch::eval::{eval, eval_file};
use rusch::io::StdinReader;
use rusch::parser::read_sexpr;
use rusch::scheme::root_env;

fn main() {
    let args = &mut env::args().skip(1);

    let env = &mut root_env();

    if args.len() == 0 {
        println!("Press ^C to exit.\n");

        let reader = &mut StdinReader::new().unwrap();
        loop {
            match read_sexpr(reader) {
                Ok(ref sexpr) => match eval(sexpr, env) {
                    Ok(result) => println!("{}", result),
                    Err(msg) => println!("Error: {}", msg),
                },
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
