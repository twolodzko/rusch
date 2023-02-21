use crate::envir;
use crate::eval::eval_but_last;
use crate::scheme::procedures::*;
use crate::types::Sexpr;

mod procedures;

type Env = envir::Env<Sexpr>;

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
mod tests {
    use super::root_env;
    use crate::errors::Error;
    use crate::eval::eval;
    use crate::list::List;
    use crate::parser::read_sexpr;
    use crate::reader::StringReader;
    use crate::scheme::procedures::lambda_init;
    use crate::types::Sexpr;

    #[macro_export]
    macro_rules! parse_eval {
        ( $txt:expr, $env:expr ) => {{
            let sexpr = read_sexpr(&mut StringReader::from($txt)).unwrap();
            eval(&sexpr, $env)
        }};
    }

    /// Assert if expression is equal to the string representation
    #[macro_export]
    macro_rules! assert_eval_eq {
        ( $lhs:expr, $rhs:expr ) => {
            let env = &mut root_env();
            assert_eq!(parse_eval!($lhs, env), $rhs);
        };
    }

    #[test]
    fn quote() {
        assert_eval_eq!("(quote foo)", Ok(Sexpr::symbol("foo")));

        assert_eval_eq!("(quote)", Err(Error::WrongArgNum));
        assert_eval_eq!("(quote foo bar)", Err(Error::WrongArgNum));
    }

    #[test]
    fn car() {
        assert_eval_eq!("(car '())", Err(Error::WrongArg(Sexpr::null())));
        assert_eval_eq!("(car '(1 2 3))", Ok(Sexpr::Integer(1)));

        assert_eval_eq!("(car)", Err(Error::WrongArgNum));
        assert_eval_eq!("(car #t)", Err(Error::WrongArg(Sexpr::True)));
    }

    #[test]
    fn cdr() {
        assert_eval_eq!("(cdr '())", Err(Error::WrongArg(Sexpr::null())));
        assert_eval_eq!("(cdr '(1))", Ok(Sexpr::null()));
        assert_eval_eq!(
            "(cdr '(1 2 3))",
            Ok(Sexpr::from(vec![Sexpr::Integer(2), Sexpr::Integer(3)]))
        );

        assert_eval_eq!("(cdr)", Err(Error::WrongArgNum));
        assert_eval_eq!("(cdr #t)", Err(Error::WrongArg(Sexpr::True)));
    }

    #[test]
    fn cons() {
        use crate::list::List;

        assert_eval_eq!(
            "(cons 1 '())",
            Ok(Sexpr::List(List::from(Sexpr::Integer(1))))
        );
        assert_eval_eq!("(cons '() '())", Ok(Sexpr::List(List::from(Sexpr::null()))));
        assert_eval_eq!(
            "(cons 1 2)",
            Ok(Sexpr::from(vec![Sexpr::Integer(1), Sexpr::Integer(2)]))
        );
        assert_eval_eq!(
            "(cons 1 '(2 3))",
            Ok(Sexpr::from(vec![
                Sexpr::Integer(1),
                Sexpr::Integer(2),
                Sexpr::Integer(3)
            ]))
        );
        assert_eval_eq!(
            "(cons 1 (list 2 3))",
            Ok(Sexpr::from(vec![
                Sexpr::Integer(1),
                Sexpr::Integer(2),
                Sexpr::Integer(3)
            ]))
        );

        // errors
        assert_eval_eq!("(cons 1)", Err(Error::WrongArgNum));
        assert_eval_eq!("(cons 1 '() '())", Err(Error::WrongArgNum));
    }

    #[test]
    fn list() {
        assert_eval_eq!("(list)", Ok(Sexpr::null()));
        assert_eval_eq!(
            "(list 1 2 3)",
            Ok(Sexpr::from(vec![
                Sexpr::Integer(1),
                Sexpr::Integer(2),
                Sexpr::Integer(3)
            ]))
        );
        assert_eval_eq!(
            "(list 'foo (list 1 2 3))",
            Ok(Sexpr::from(vec![
                Sexpr::symbol("foo"),
                Sexpr::from(vec![
                    Sexpr::Integer(1),
                    Sexpr::Integer(2),
                    Sexpr::Integer(3)
                ])
            ]))
        );
        assert_eval_eq!("(list 'foo (list 1 (car) 3))", Err(Error::WrongArgNum));
    }

    #[test]
    fn add() {
        use Sexpr::{Float, Integer};

        // basics
        assert_eval_eq!("(+)", Ok(Integer(0)));
        assert_eval_eq!("(+ 2 2)", Ok(Integer(4)));
        assert_eval_eq!("(+ '2 2)", Ok(Integer(4)));
        assert_eval_eq!("(+ 1 2 3)", Ok(Integer(6)));
        assert_eval_eq!("(+ 1 (+ 1 1) (+ 1 1 1))", Ok(Integer(6)));

        // floats & casts
        assert_eval_eq!("(+ 2.0  2.0 )", Ok(Float(4.0)));
        assert_eval_eq!("(+  2   2.0 )", Ok(Float(4.0)));
        assert_eval_eq!("(+ 2.0   2  )", Ok(Float(4.0)));

        // errors
        assert_eval_eq!("(+ x)", Err(Error::NotFound(String::from("x"))));
        assert_eval_eq!("(+ 'foo)", Err(Error::NotANumber(Sexpr::symbol("foo"))));
        assert_eval_eq!("(+ 1 'foo)", Err(Error::NotANumber(Sexpr::symbol("foo"))));
        assert_eval_eq!("(+ \"1\" 2 3 4)", Err(Error::NotANumber(Sexpr::from("1"))));
    }

    #[test]
    fn sub() {
        use Sexpr::{Float, Integer};

        // basics
        assert_eval_eq!("(-)", Ok(Integer(0)));
        assert_eval_eq!("(- 1)", Ok(Integer(-1)));
        assert_eval_eq!("(- 3 2)", Ok(Integer(1)));
        assert_eval_eq!("(- '3 2)", Ok(Integer(1)));
        assert_eval_eq!("(- 10 5 2)", Ok(Integer(3)));
        assert_eval_eq!("(- 6 (- 2 1) (- 6 3 1))", Ok(Integer(3)));

        // floats & casts
        assert_eval_eq!("(- 2.0  1.0 )", Ok(Float(1.0)));
        assert_eval_eq!("(-  2   1.0 )", Ok(Float(1.0)));
        assert_eval_eq!("(- 2.0   1  )", Ok(Float(1.0)));

        // errors
        assert_eval_eq!("(- 1 'foo)", Err(Error::NotANumber(Sexpr::symbol("foo"))));
        assert_eval_eq!("(- \"1\" 2 3 4)", Err(Error::NotANumber(Sexpr::from("1"))));
    }

    #[test]
    fn mul() {
        use Sexpr::{Float, Integer};

        // basics
        assert_eval_eq!("(*)", Ok(Integer(1)));
        assert_eval_eq!("(* 2 3)", Ok(Integer(6)));
        assert_eval_eq!("(* '2 3)", Ok(Integer(6)));
        assert_eval_eq!("(* 1 2 3)", Ok(Integer(6)));
        assert_eval_eq!("(* 1 (* 1 2) (* 1 2 3))", Ok(Integer(12)));

        // floats & casts
        assert_eval_eq!("(* 2.0  3.0 )", Ok(Float(6.0)));
        assert_eval_eq!("(*  2   3.0 )", Ok(Float(6.0)));
        assert_eval_eq!("(* 2.0   3  )", Ok(Float(6.0)));

        // errors
        assert_eval_eq!("(* 1 'foo)", Err(Error::NotANumber(Sexpr::symbol("foo"))));
        assert_eval_eq!("(* \"1\" 2 3 4)", Err(Error::NotANumber(Sexpr::from("1"))));
    }

    #[test]
    fn div() {
        use Sexpr::Float;

        // basics
        assert_eval_eq!("(/ 2)", Ok(Float(0.5)));
        assert_eval_eq!("(/ '9 3)", Ok(Float(3.0)));
        assert_eval_eq!("(/ 30 10 3)", Ok(Float(1.0)));
        assert_eval_eq!("(/ 300 (/ 12 4) (/ 500 5 10))", Ok(Float(10.0)));

        // // floats & casts
        assert_eval_eq!("(/ 6.0  3.0 )", Ok(Float(2.0)));
        assert_eval_eq!("(/  6   3.0 )", Ok(Float(2.0)));
        assert_eval_eq!("(/ 6.0   3  )", Ok(Float(2.0)));

        // errors
        assert_eval_eq!("(/)", Err(Error::WrongArgNum));
        assert_eval_eq!("(/ 1 'foo)", Err(Error::NotANumber(Sexpr::symbol("foo"))));
        assert_eval_eq!("(/ \"1\" 2 3 4)", Err(Error::NotANumber(Sexpr::from("1"))));
    }

    #[test]
    fn is_equal() {
        use crate::types::Sexpr::{False, True};

        // positive
        assert_eval_eq!("(equal?)", Ok(True));
        assert_eval_eq!("(equal? #t)", Ok(True));
        assert_eval_eq!("(equal? #f)", Ok(True));
        assert_eval_eq!("(equal? 2 2)", Ok(True));
        assert_eval_eq!("(equal? '() '())", Ok(True));
        assert_eval_eq!("(equal? '() (list) (cdr '('head)))", Ok(True));
        assert_eval_eq!("(equal? '(1 2 3) (list 1 (+ 1 1) (+ 1 2)))", Ok(True));
        assert_eval_eq!("(equal? 2 2.0 (+ 1 1) (/ 6 3))", Ok(True));

        // negative
        assert_eval_eq!("(equal? 1 2)", Ok(False));
        assert_eval_eq!("(equal? 2 \"2\")", Ok(False));
        assert_eval_eq!("(equal? \"foo\" 'foo)", Ok(False));
        assert_eval_eq!("(equal? (list 1 2 3) (list 1 2))", Ok(False));
        assert_eval_eq!("(equal? (list 1 2 3) (list 1 2 3 4))", Ok(False));
    }

    #[test]
    fn cmp() {
        use crate::types::Sexpr::{False, True};

        // positive
        assert_eval_eq!("(<)", Ok(True));
        assert_eval_eq!("(< 1)", Ok(True));
        assert_eval_eq!("(< 1 2)", Ok(True));
        assert_eval_eq!("(< 1 2 3)", Ok(True));
        assert_eval_eq!("(< 1 (+ 1 2) (+ 1 (+ 1 2)))", Ok(True));

        assert_eval_eq!("(>)", Ok(True));
        assert_eval_eq!("(> 1)", Ok(True));
        assert_eval_eq!("(> 3 2 1)", Ok(True));

        // negative
        assert_eval_eq!("(< 1 3 2)", Ok(False));
        assert_eval_eq!("(> 3 1 2)", Ok(False));
        assert_eval_eq!("(< 1 2 3 2 5)", Ok(False));
    }

    #[test]
    fn iffn() {
        use crate::types::Sexpr::Integer;

        assert_eval_eq!("(if #t 1 2)", Ok(Integer(1)));
        assert_eval_eq!("(if #f 1 2)", Ok(Integer(2)));
        assert_eval_eq!("(if #t (+ 1 2) (+ 3 4))", Ok(Integer(3)));
        assert_eval_eq!("(if #f (+ 1 2) (+ 3 4))", Ok(Integer(7)));
        assert_eval_eq!("(if #t 1 (/ 'foo 'bar))", Ok(Integer(1)));
        assert_eval_eq!("(if #f (/ 'foo 'bar) 2)", Ok(Integer(2)));
        assert_eval_eq!("(if (= 2 2.0) 1 2)", Ok(Integer(1)));
        assert_eval_eq!("(if (= #t #f) 1 2)", Ok(Integer(2)));

        // errors
        assert_eval_eq!("(if #t)", Err(Error::WrongArgNum));
        assert_eval_eq!("(if #f 1)", Err(Error::WrongArgNum));
    }

    #[test]
    fn condfn() {
        assert_eval_eq!("(cond)", Ok(Sexpr::Nil));
        assert_eval_eq!("(cond (#t))", Ok(Sexpr::True));
        assert_eval_eq!("(cond ((= 2 2)))", Ok(Sexpr::True));
        assert_eval_eq!("(cond (#f 1) (#t 2))", Ok(Sexpr::Integer(2)));
        assert_eval_eq!(
            "(cond (#f (error 'oh 'no)) (#t (->integer (/ 4 2))))",
            Ok(Sexpr::Integer(2))
        );
        assert_eval_eq!(
            "(cond (#f (error 'first 'err)) (#t 'ok) (#t (error 'second 'err)))",
            Ok(Sexpr::symbol("ok"))
        );

        // errors
        assert_eval_eq!("(cond ())", Err(Error::WrongArgNum));
        assert_eval_eq!("(cond #t)", Err(Error::WrongArg(Sexpr::True)));
    }

    #[test]
    fn define() {
        let mut env = root_env();

        assert!(parse_eval!("(define x 'foo)", &mut env).is_ok());
        assert_eq!(env.get(&String::from("x")), Some(Sexpr::symbol("foo")));

        assert!(parse_eval!("(define x 'bar)", &mut env).is_ok());
        assert_eq!(env.get(&String::from("x")), Some(Sexpr::symbol("bar")));

        assert!(parse_eval!("(define y x)", &mut env).is_ok());
        assert_eq!(env.get(&String::from("y")), Some(Sexpr::symbol("bar")));

        assert!(parse_eval!("(define y (+ 2 (/ 4 2)))", &mut env).is_ok());
        assert_eq!(env.get(&String::from("y")), Some(Sexpr::Float(4.0)));

        assert_eq!(env.get(&String::from("x")), Some(Sexpr::symbol("bar")));

        // errors
        assert_eq!(
            parse_eval!("(define \"x\" 'foo)", &mut env),
            Err(Error::WrongArg(Sexpr::from("x")))
        );
        assert_eq!(parse_eval!("(define)", &mut env), Err(Error::WrongArgNum));
        assert_eq!(parse_eval!("(define x)", &mut env), Err(Error::WrongArgNum));
        assert_eq!(
            parse_eval!("(define x 'foo 'bar)", &mut env),
            Err(Error::WrongArgNum)
        );
    }

    #[test]
    fn define_function() {
        let mut env = root_env();

        assert!(parse_eval!("(define (yes) #t)", &mut env).is_ok());
        assert_eq!(
            env.get(&String::from("yes")),
            Some(lambda_init(&List::empty(), &List::from(vec![Sexpr::True]), &mut env).unwrap())
        );

        assert!(parse_eval!("(define (add1 x) (+ x 1))", &mut env).is_ok());
        assert_eq!(
            env.get(&String::from("add1")),
            Some(
                lambda_init(
                    &List::from(vec![Sexpr::symbol("x")]),
                    &List::from(vec![Sexpr::List(List::from(vec![
                        Sexpr::symbol("+"),
                        Sexpr::symbol("x"),
                        Sexpr::Integer(1),
                    ]))]),
                    &mut env
                )
                .unwrap()
            )
        );

        assert!(parse_eval!("(define (foo x y) (display x '+ y '=) (+ x y))", &mut env).is_ok());
        assert_eq!(
            env.get(&String::from("foo")),
            Some(
                lambda_init(
                    &List::from(vec![Sexpr::symbol("x"), Sexpr::symbol("y")]),
                    &List::from(vec![
                        Sexpr::from(vec![
                            Sexpr::symbol("display"),
                            Sexpr::symbol("x"),
                            Sexpr::from(vec![Sexpr::symbol("quote"), Sexpr::symbol("+")]),
                            Sexpr::symbol("y"),
                            Sexpr::from(vec![Sexpr::symbol("quote"), Sexpr::symbol("=")]),
                        ]),
                        Sexpr::from(vec![
                            Sexpr::symbol("+"),
                            Sexpr::symbol("x"),
                            Sexpr::symbol("y")
                        ])
                    ]),
                    &mut env
                )
                .unwrap()
            )
        );

        // errors
        assert_eq!(
            parse_eval!("(define ())", &mut env),
            Err(Error::WrongArgNum)
        );
        assert_eq!(
            parse_eval!("(define (foo))", &mut env),
            Err(Error::WrongArgNum)
        );
        assert_eq!(
            parse_eval!("(define (#t) #f)", &mut env),
            Err(Error::WrongArg(Sexpr::True))
        );
    }

    #[test]
    fn lambda() {
        assert_eval_eq!("((lambda (x) x) 42)", Ok(Sexpr::Integer(42)));
        assert_eval_eq!("((lambda (x y) (+ x y)) 5 6)", Ok(Sexpr::Integer(11)));
        assert_eval_eq!(
            "((lambda (x) ((lambda (y) (+ x y)) x)) 2)",
            Ok(Sexpr::Integer(4))
        );

        // errors
        assert_eval_eq!("(lambda (5) x)", Err(Error::WrongArg(Sexpr::Integer(5))));
        assert_eval_eq!("(lambda (x #t y) x)", Err(Error::WrongArg(Sexpr::True)));
        assert_eval_eq!(
            "(lambda (\"x\") x)",
            Err(Error::WrongArg(Sexpr::String(String::from("x"))))
        );
        assert_eval_eq!("((lambda (x y) (+ x y)) 1)", Err(Error::WrongArgNum));
        assert_eval_eq!("((lambda (x) x) 1 2)", Err(Error::WrongArgNum));
    }

    #[test]
    fn let_core() {
        assert_eval_eq!("(let () 'ok)", Ok(Sexpr::symbol("ok")));
        assert_eval_eq!("(let ((x 1) (y 2)) (+ x y))", Ok(Sexpr::Integer(3)));
        assert_eval_eq!("(let ((x 1) (y 2)) (+ x y) (* y y))", Ok(Sexpr::Integer(4)));
        assert_eval_eq!("(let ((x 1)) (let ((y x)) (+ x y)))", Ok(Sexpr::Integer(2)));

        // errors
        assert_eval_eq!("(let 42 '())", Err(Error::WrongArg(Sexpr::Integer(42))));
        assert_eval_eq!(
            "(let ((x 1) y 2) (+ x y))",
            Err(Error::WrongArg(Sexpr::symbol("y")))
        );

        // it's not let*
        assert_eval_eq!(
            "(let ((x 1) (y (+ x 1))) (+ x y))",
            Err(Error::NotFound(String::from("x")))
        );
    }

    #[test]
    fn named_let() {
        assert_eval_eq!("(let f ())", Ok(Sexpr::Nil));
        assert_eval_eq!(
            "(let rev ((ls '(a b c)) (new '())) \
                (if (null? ls) \
                    new \
                    (rev (cdr ls) (cons (car ls) new))))",
            Ok(Sexpr::from(vec![
                Sexpr::symbol("c"),
                Sexpr::symbol("b"),
                Sexpr::symbol("a")
            ]))
        );
        assert_eval_eq!(
            "(let fac ((n 0)) (if (= n 0) 1 (* n (fac (- n 1)))))",
            Ok(Sexpr::Integer(1))
        );
        assert_eval_eq!(
            "(let fac ((n 10)) (if (= n 0) 1 (* n (fac (- n 1)))))",
            Ok(Sexpr::Integer(3628800))
        );
        assert_eval_eq!(
            "(let fib ((i 10)) (cond ((= i 0) 0) ((= i 1) 1) (else (+ (fib (- i 1)) (fib (- i 2))))))",
            Ok(Sexpr::Integer(55))
        );
    }

    #[test]
    fn closures() {
        assert_eval_eq!(
            "(((lambda (x) (lambda (y) (/ x y))) 1) 2)",
            Ok(Sexpr::Float(0.5))
        );
        assert_eval_eq!(
            "((lambda (x) (let ((x (+ x 1))) (* x 2))) 1)",
            Ok(Sexpr::Integer(4))
        );
        assert_eval_eq!(
            "(let ((x 2)) ((lambda (y) (+ y (+ x 1))) x))",
            Ok(Sexpr::Integer(5))
        );
        assert_eval_eq!(
            "((let ((x 2)) (lambda (y) (+ x y))) 3)",
            Ok(Sexpr::Integer(5))
        );
        assert_eval_eq!(
            "((let* ((x 1) (z (+ x 1))) (lambda (y) (+ x y z))) 3)",
            Ok(Sexpr::Integer(6))
        );
    }

    #[test]
    fn let_star() {
        // Works like let
        assert_eval_eq!("(let* () 'ok)", Ok(Sexpr::symbol("ok")));
        assert_eval_eq!("(let* ((x 1) (y 2)) (+ x y))", Ok(Sexpr::Integer(3)));
        assert_eval_eq!(
            "(let* ((x 1) (y 2)) (+ x y) (* y y))",
            Ok(Sexpr::Integer(4))
        );
        assert_eval_eq!(
            "(let* ((x 1)) (let* ((y x)) (+ x y)))",
            Ok(Sexpr::Integer(2))
        );
        assert_eval_eq!(
            "(let* ((x 1) (y (- 3 x))) (car '(1 2 3)) (+ x y))",
            Ok(Sexpr::Integer(3))
        );

        // let* recursive evaluation
        assert_eval_eq!("(let* ((x 1) (y (+ x 1))) (+ x y))", Ok(Sexpr::Integer(3)));
    }

    #[test]
    fn logic() {
        assert_eval_eq!("(not #f)", Ok(Sexpr::True));
        assert_eval_eq!("(not)", Ok(Sexpr::False));
        assert_eval_eq!("(not #t)", Ok(Sexpr::False));
        assert_eval_eq!("(not '())", Ok(Sexpr::False));
        assert_eval_eq!("(not 0)", Ok(Sexpr::False));

        assert_eval_eq!("(and)", Ok(Sexpr::True));
        assert_eval_eq!("(and #t)", Ok(Sexpr::True));
        assert_eval_eq!("(and #t '() 42 (= 2 (+ 1 1)))", Ok(Sexpr::True));
        assert_eval_eq!("(and #t '() #f 42)", Ok(Sexpr::False));
        assert_eval_eq!("(and #t 1 2 3)", Ok(Sexpr::Integer(3)));

        assert_eval_eq!("(or)", Ok(Sexpr::False));
        assert_eval_eq!("(or #f)", Ok(Sexpr::False));
        assert_eval_eq!("(or #t)", Ok(Sexpr::True));
        assert_eval_eq!("(or #f (= 5 6) #t #f)", Ok(Sexpr::True));
        assert_eval_eq!("(or #f (+ 2 2) #t #f)", Ok(Sexpr::Integer(4)));
    }

    #[test]
    fn type_checkers() {
        assert_eval_eq!("(bool? #t)", Ok(Sexpr::True));
        assert_eval_eq!("(bool? #f)", Ok(Sexpr::True));
        assert_eval_eq!("(bool? (= 2 2))", Ok(Sexpr::True));
        assert_eval_eq!("(bool? 0)", Ok(Sexpr::False));
        assert_eval_eq!("(bool? 'true)", Ok(Sexpr::False));
        assert_eval_eq!("(bool? '())", Ok(Sexpr::False));

        assert_eval_eq!("(symbol? 'foo)", Ok(Sexpr::True));
        assert_eval_eq!("(symbol? \"foo\")", Ok(Sexpr::False));

        assert_eval_eq!("(string? \"hello world!\")", Ok(Sexpr::True));
        assert_eval_eq!("(string? 'hello)", Ok(Sexpr::False));

        assert_eval_eq!("(number? -5)", Ok(Sexpr::True));
        assert_eval_eq!("(number? 3.14)", Ok(Sexpr::True));
        assert_eval_eq!("(number? (/ 5 (- 100 (* 7 2))))", Ok(Sexpr::True));
        assert_eval_eq!("(number? \"42\")", Ok(Sexpr::False));
        assert_eval_eq!("(number? #t)", Ok(Sexpr::False));
        assert_eval_eq!("(number? '())", Ok(Sexpr::False));

        assert_eval_eq!("(integer? 42)", Ok(Sexpr::True));
        assert_eval_eq!("(integer? 3.14)", Ok(Sexpr::False));
        assert_eval_eq!("(integer? 'foo)", Ok(Sexpr::False));

        assert_eval_eq!("(float? 3.14)", Ok(Sexpr::True));
        assert_eval_eq!("(float? 42)", Ok(Sexpr::False));
        assert_eval_eq!("(float? 'foo)", Ok(Sexpr::False));

        assert_eval_eq!("(null? '())", Ok(Sexpr::True));
        assert_eval_eq!("(null? (list))", Ok(Sexpr::True));
        assert_eval_eq!("(null? (cdr '(1)))", Ok(Sexpr::True));
        assert_eval_eq!("(null? (list 1 2 3))", Ok(Sexpr::False));
        assert_eval_eq!("(null? '(1 2 3))", Ok(Sexpr::False));
        assert_eval_eq!("(null? '(()))", Ok(Sexpr::False));
        assert_eval_eq!("(null? #t)", Ok(Sexpr::False));

        assert_eval_eq!("(pair? (list 1 2 3))", Ok(Sexpr::True));
        assert_eval_eq!("(pair? '(1))", Ok(Sexpr::True));
        assert_eval_eq!("(pair? '())", Ok(Sexpr::False));
        assert_eval_eq!("(pair? 'foo)", Ok(Sexpr::False));

        assert_eval_eq!("(procedure? car)", Ok(Sexpr::True));
        assert_eval_eq!("(procedure? (lambda (x) x))", Ok(Sexpr::True));
        assert_eval_eq!("(procedure? 'car)", Ok(Sexpr::False));
    }

    #[test]
    fn conversions() {
        assert_eval_eq!("(->integer 42)", Ok(Sexpr::Integer(42)));
        assert_eval_eq!("(->integer 3.14)", Ok(Sexpr::Integer(3)));
        assert_eval_eq!("(->integer (/ 10 3))", Ok(Sexpr::Integer(3)));
        assert_eval_eq!("(->integer \"-53\")", Ok(Sexpr::Integer(-53)));
        assert_eval_eq!(
            "(->integer 'foo)",
            Err(Error::CannotParse(Sexpr::symbol("foo")))
        );

        assert_eval_eq!("(->float 3.14)", Ok(Sexpr::Float(3.14)));
        assert_eval_eq!("(->float 5)", Ok(Sexpr::Float(5.0)));
        assert_eval_eq!("(->float (+ 10 1))", Ok(Sexpr::Float(11.0)));
        assert_eval_eq!("(->float \"365\")", Ok(Sexpr::Float(365.0)));
        assert_eval_eq!("(->float '())", Err(Error::CannotParse(Sexpr::null())));
    }

    #[test]
    fn set() {
        let root = &mut root_env();
        let mut local1 = root.branch();
        local1.insert(&String::from("foo"), Sexpr::Integer(42));
        let mut local2 = local1.branch();

        assert_eq!(local2.get(&String::from("foo")), Some(Sexpr::Integer(42)));
        assert_eq!(
            parse_eval!("(set! foo (car (list 'bar)))", &mut local2),
            Ok(Sexpr::Nil)
        );

        assert_eq!(local2.get(&String::from("foo")), Some(Sexpr::symbol("bar")));
        assert_eq!(local1.get(&String::from("foo")), Some(Sexpr::symbol("bar")));
        assert_eq!(root.get(&String::from("foo")), None);

        assert_eq!(
            parse_eval!("(set! pi 3.14)", &mut local2),
            Err(Error::NotFound(String::from("pi")))
        );
    }

    #[test]
    fn evalfn() {
        assert_eval_eq!("(eval ''foo)", Ok(Sexpr::symbol("foo")));
        assert_eval_eq!("(eval '(+ 2 2))", Ok(Sexpr::Integer(4)));
    }

    #[test]
    fn to_string() {
        assert_eval_eq!("(string)", Ok(Sexpr::from("")));
        assert_eval_eq!(
            "(string 1 #t '() 'foo (+ 2 2) \"hello!\")",
            Ok(Sexpr::String(String::from("1 #t () foo 4 \"hello!\"")))
        );
    }

    #[test]
    fn to_error() {
        assert_eval_eq!("(error)", Err(Error::from("")));
        assert_eval_eq!("(error 'hello 'world)", Err(Error::from("hello world")));
    }

    #[test]
    fn begin() {
        let root = &mut root_env();

        assert_eq!(parse_eval!("(begin)", root), Ok(Sexpr::Nil));

        assert_eq!(
            parse_eval!("(begin (define foo 'bar) (+ 5 6) (+ 2 2))", root),
            Ok(Sexpr::Integer(4))
        );
        assert_eq!(root.get(&String::from("foo")), Some(Sexpr::symbol("bar")));

        // break evaluation on error
        assert_eq!(
            parse_eval!(
                "(begin (/ 70 5) (error 'expected) (define dont #f) (+ 2 2))",
                root
            ),
            Err(Error::from("expected"))
        );
        assert_eq!(root.get(&String::from("dont")), None);
    }

    #[test]
    fn fibo_vanilla() {
        let env = &mut root_env();

        assert!(parse_eval!(
            "(define fibo (lambda (n)
                (if (= n 0) 0
                    (if (= n 1) 1
                        (+ (fibo (- n 1))
                        (fibo (- n 2)))))))",
            env
        )
        .is_ok());

        assert_eq!(parse_eval!("(fibo 0)", env), Ok(Sexpr::Integer(0)));
        assert_eq!(parse_eval!("(fibo 1)", env), Ok(Sexpr::Integer(1)));
        assert_eq!(parse_eval!("(fibo 2)", env), Ok(Sexpr::Integer(1)));
        assert_eq!(parse_eval!("(fibo 3)", env), Ok(Sexpr::Integer(2)));
        assert_eq!(parse_eval!("(fibo 7)", env), Ok(Sexpr::Integer(13)));
        assert_eq!(parse_eval!("(fibo 9)", env), Ok(Sexpr::Integer(34)));
        assert_eq!(parse_eval!("(fibo 10)", env), Ok(Sexpr::Integer(55)));
    }

    #[test]
    fn reverse() {
        assert_eval_eq!("(reverse '())", Ok(Sexpr::null()));
        assert_eval_eq!(
            "(reverse '(a b c))",
            Ok(Sexpr::from(vec![
                Sexpr::symbol("c"),
                Sexpr::symbol("b"),
                Sexpr::symbol("a"),
            ]))
        );
        assert_eval_eq!(
            "(reverse (list 'foo (+ 2 2) #t))",
            Ok(Sexpr::from(vec![
                Sexpr::True,
                Sexpr::Integer(4),
                Sexpr::symbol("foo"),
            ]))
        );
    }

    #[test]
    fn fibo_tail_recur() {
        let env = &mut root_env();

        assert!(parse_eval!(
            "(define impl (lambda (it second first)
                (if (= it 0) first
                    (impl (- it 1) (+ first second) second))))",
            env
        )
        .is_ok());
        assert!(parse_eval!("(define fibo (lambda (n) (impl n 1.0 0.0)))", env).is_ok());

        assert_eq!(parse_eval!("(fibo 0)", env), Ok(Sexpr::Integer(0)));
        assert_eq!(parse_eval!("(fibo 1)", env), Ok(Sexpr::Integer(1)));
        assert_eq!(parse_eval!("(fibo 2)", env), Ok(Sexpr::Integer(1)));
        assert_eq!(parse_eval!("(fibo 3)", env), Ok(Sexpr::Integer(2)));
        assert_eq!(parse_eval!("(fibo 7)", env), Ok(Sexpr::Integer(13)));
        assert_eq!(parse_eval!("(fibo 9)", env), Ok(Sexpr::Integer(34)));
        assert_eq!(parse_eval!("(fibo 10)", env), Ok(Sexpr::Integer(55)));

        // this would fail without tail-call optimization
        let _ = parse_eval!("(fibo 1000)", env);
    }

    #[test]
    fn load() {
        let env = &mut root_env();
        assert_eq!(
            parse_eval!("(load \"examples/simple.scm\")", env),
            Ok(Sexpr::Integer(321))
        );
        assert_eq!(env.get(&String::from("x")), Some(Sexpr::Integer(1)));
    }
}
