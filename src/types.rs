use std::cmp::Ordering;
use std::fmt;

use crate::envir;
use crate::errors::Error;
use crate::list::List;

#[derive(Clone)]
pub enum Sexpr {
    True,
    False,
    Symbol(String),
    Integer(i64),
    Float(f64),
    String(String),
    List(List<Sexpr>),
    Func(Func),
    Tco(TcoFunc),
    Lambda(Box<Lambda>),
    Nil,
}

pub type Args = List<Sexpr>;
pub type Env = envir::Env<Sexpr>;

pub type FuncResult = Result<Sexpr, Error<Sexpr>>;
pub type Func = fn(&Args, &mut Env) -> FuncResult;

pub type TcoResult = Result<(Sexpr, Option<Env>), Error<Sexpr>>;
pub type TcoFunc = fn(&Args, &mut Env) -> TcoResult;

#[derive(Clone, PartialEq)]
pub struct Lambda {
    pub vars: Vec<String>,
    pub body: List<Sexpr>,
    pub env: Env,
}

impl Sexpr {
    #[inline]
    pub fn null() -> Sexpr {
        Sexpr::List(List::empty())
    }

    #[inline]
    pub fn symbol(name: &str) -> Sexpr {
        Sexpr::Symbol(String::from(name))
    }

    #[inline]
    pub fn lambda(vars: Vec<String>, body: List<Sexpr>, env: Env) -> Self {
        Sexpr::Lambda(Box::new(Lambda { vars, body, env }))
    }

    #[inline]
    pub fn is_true(&self) -> bool {
        // see: https://www.scheme.com/tspl4/intro.html#./intro:s36
        !matches!(self, Sexpr::False)
    }

    #[inline]
    pub fn is_number(&self) -> bool {
        matches!(self, Sexpr::Integer(_) | Sexpr::Float(_))
    }
}

impl fmt::Display for List<Sexpr> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let stringified = self
            .iter()
            .map(|elem| elem.to_string())
            .collect::<Vec<String>>()
            .join(" ");
        write!(f, "({})", stringified)
    }
}

impl fmt::Display for Sexpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Sexpr::*;
        match self {
            True => write!(f, "#t"),
            False => write!(f, "#f"),
            Symbol(ref value) => write!(f, "{}", value),
            String(ref value) => write!(f, "\"{}\"", value),
            Float(value) => value.fmt(f),
            Integer(value) => value.fmt(f),
            List(ref list) => list.fmt(f),
            Func(ref func) => write!(f, "Func<{:#x}>", *func as usize),
            Tco(ref func) => write!(f, "Func<{:#x}>", *func as usize),
            Lambda(ref lambda) => lambda.fmt(f),
            Nil => write!(f, "<nil>"),
        }
    }
}

impl std::fmt::Debug for Sexpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl std::cmp::PartialEq for Sexpr {
    fn eq(&self, other: &Self) -> bool {
        use Sexpr::*;
        match (self, other) {
            // basic types
            (True, True) => true,
            (False, False) => true,
            (String(s), String(o)) => s == o,
            (Symbol(s), Symbol(o)) => s == o,
            // numbers
            (Integer(s), Integer(o)) => s == o,
            (Float(s), Float(o)) => s == o,
            (Integer(s), Float(o)) => &(*s as f64) == o,
            (Float(s), Integer(o)) => s == &(*o as f64),
            // lists
            (List(s), List(o)) => s == o,
            // functions are compared by memory addresses
            (Func(s), Func(o)) => *s as usize == *o as usize,
            (Tco(s), Tco(o)) => *s as usize == *o as usize,
            // lambdas
            (Lambda(s), Lambda(o)) => s == o,
            // Nil's
            (Nil, Nil) => true,
            // non-matching types
            _ => false,
        }
    }
}

impl std::cmp::PartialOrd for Sexpr {
    fn partial_cmp(&self, other: &Sexpr) -> Option<Ordering> {
        match (self, other) {
            (Sexpr::Integer(x), Sexpr::Integer(y)) => x.partial_cmp(y),
            (Sexpr::Integer(x), Sexpr::Float(y)) => (*x as f64).partial_cmp(y),
            (Sexpr::Float(x), Sexpr::Integer(y)) => x.partial_cmp(&(*y as f64)),
            (Sexpr::Float(x), Sexpr::Float(y)) => x.partial_cmp(y),
            _ => None,
        }
    }
}

impl From<bool> for Sexpr {
    fn from(val: bool) -> Self {
        match val {
            true => Self::True,
            false => Self::False,
        }
    }
}

impl From<&str> for Sexpr {
    fn from(val: &str) -> Self {
        Sexpr::String(String::from(val))
    }
}

impl From<Vec<Sexpr>> for Sexpr {
    fn from(vec: Vec<Sexpr>) -> Self {
        Sexpr::List(List::from(vec))
    }
}

impl FromIterator<Sexpr> for Sexpr {
    fn from_iter<I: IntoIterator<Item = Sexpr>>(iter: I) -> Self {
        Sexpr::List(List::<Sexpr>::from_iter(iter))
    }
}

impl fmt::Display for Lambda {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let body = self
            .body
            .iter()
            .map(|elem| elem.to_string())
            .collect::<Vec<String>>()
            .join(" ");
        let args = self.vars.join(" ");
        if body.is_empty() {
            return write!(f, "(lambda ({}))", args);
        }
        write!(f, "(lambda ({}) {})", args, body)
    }
}

impl From<Sexpr> for List<Sexpr> {
    #[inline]
    fn from(elem: Sexpr) -> Self {
        List::empty().push_front(elem)
    }
}

#[cfg(test)]
mod tests {
    use super::{Lambda, Sexpr};
    use crate::envir::Env;
    use crate::list::List;

    /// Assert if expression is equal to the string representation
    #[macro_export]
    macro_rules! assert_fmt_eq {
        ( $x:expr, $y:literal ) => {
            assert_eq!(&format!("{}", $x), $y)
        };
    }

    #[test]
    fn fmt_true() {
        assert_fmt_eq!(Sexpr::True, "#t")
    }

    #[test]
    fn fmt_false() {
        assert_fmt_eq!(Sexpr::False, "#f")
    }

    #[test]
    fn fmt_symbol() {
        assert_fmt_eq!(Sexpr::symbol("foo"), "foo")
    }

    #[test]
    fn fmt_string() {
        assert_fmt_eq!(Sexpr::from("hello world"), "\"hello world\"")
    }

    #[test]
    fn fmt_integer() {
        assert_fmt_eq!(Sexpr::Integer(42), "42")
    }

    #[test]
    fn fmt_float() {
        assert_fmt_eq!(Sexpr::Float(-3.14), "-3.14")
    }

    #[test]
    fn fmt_list() {
        assert_fmt_eq!(
            Sexpr::from(vec![
                Sexpr::Integer(1),
                Sexpr::Integer(2),
                Sexpr::Integer(3)
            ]),
            "(1 2 3)"
        );
        assert_fmt_eq!(
            Sexpr::List(List::from(vec![
                Sexpr::from(vec![
                    Sexpr::Integer(1),
                    Sexpr::Integer(2),
                    Sexpr::Integer(3)
                ]),
                Sexpr::from(vec![Sexpr::Integer(4), Sexpr::Integer(5)])
            ])),
            "((1 2 3) (4 5))"
        );

        assert_fmt_eq!(
            Sexpr::from(vec![
                Sexpr::symbol("list"),
                Sexpr::Float(-3.14),
                Sexpr::Integer(42),
                Sexpr::False,
                Sexpr::null(),
            ]),
            "(list -3.14 42 #f ())"
        )
    }

    #[test]
    fn fmt_lambda() {
        assert_fmt_eq!(
            Sexpr::Lambda(Box::new(Lambda {
                vars: Vec::<String>::new(),
                body: List::empty(),
                env: Env::new(),
            })),
            "(lambda ())"
        );

        assert_fmt_eq!(
            Sexpr::Lambda(Box::new(Lambda {
                vars: vec![String::from("x")],
                body: List::from(vec![Sexpr::symbol("x")]),
                env: Env::new(),
            })),
            "(lambda (x) x)"
        );

        assert_fmt_eq!(
            Sexpr::Lambda(Box::new(Lambda {
                vars: vec![String::from("x"), String::from("y")],
                body: List::from(vec![Sexpr::List(List::from(vec![
                    Sexpr::symbol("+"),
                    Sexpr::symbol("x"),
                    Sexpr::symbol("y")
                ]))]),
                env: Env::new(),
            })),
            "(lambda (x y) (+ x y))"
        );
    }

    #[test]
    fn is_true() {
        assert!(Sexpr::True.is_true());
        assert!(!Sexpr::False.is_true());
        assert!(Sexpr::null().is_true());
        assert!(Sexpr::symbol("true").is_true());
    }

    #[test]
    fn comparisons() {
        use Sexpr::*;

        assert!(True == True);
        assert!(True != False);
        assert!(True != Integer(1));
        assert!(True != Sexpr::null());
        assert!(Sexpr::null() == Sexpr::null());
        assert!(Sexpr::symbol("foo") == Sexpr::symbol("foo"));
        assert!(Sexpr::symbol("foo") != Sexpr::symbol("bar"));
        assert!(
            Sexpr::from(vec![Integer(1), Sexpr::symbol("foo"), False])
                == Sexpr::from(vec![Integer(1), Sexpr::symbol("foo"), False])
        );
        assert!(
            Sexpr::from(vec![Integer(1), Sexpr::symbol("foo"), False])
                != Sexpr::from(vec![Integer(1), Sexpr::symbol("foo")])
        );
        assert!(
            Sexpr::from(vec![Integer(1), Sexpr::symbol("foo")])
                != Sexpr::from(vec![Integer(1), Sexpr::symbol("foo"), False])
        );
        assert!(Sexpr::from("hello") == Sexpr::from("hello"));
        assert!(Sexpr::from("hello") != Sexpr::from("hello!"));
        assert!(Integer(42) == Integer(42));
        assert!(Integer(42) != Integer(43));
        assert!(Float(3.14) == Float(3.14));
        assert!(Float(1.0) == Integer(1));
        assert!(Integer(1) == Float(1.0));
        assert!(Integer(1) != Sexpr::symbol("1"));
        assert!(Sexpr::symbol("1") != Integer(1));

        assert!(Integer(2) > Integer(1));
        assert!(Integer(2) >= Integer(1));
        assert!(Float(2.0) > Integer(1));
        assert!(Integer(2) > Float(1.0));
        assert!(Integer(1) < Integer(2));
        assert!(Integer(100) <= Integer(200));

        let first = Sexpr::Func(|_, _| Ok(Sexpr::True));
        let second = Sexpr::Func(|_, _| Ok(Sexpr::False));
        assert!(first == first);
        assert!(first == first.clone());
        assert!(second == second);
        assert!(first != second);
        assert!(first != Sexpr::True);
    }

    #[test]
    fn from() {
        assert_eq!(Sexpr::from(true), Sexpr::True);
        assert_eq!(Sexpr::from(false), Sexpr::False);
        assert_eq!(
            Sexpr::from("hello world"),
            Sexpr::String(String::from("hello world"))
        );
        assert_eq!(
            Sexpr::from(vec![
                Sexpr::Integer(1),
                Sexpr::Integer(2),
                Sexpr::Integer(3)
            ]),
            Sexpr::List(List::from(vec![
                Sexpr::Integer(1),
                Sexpr::Integer(2),
                Sexpr::Integer(3)
            ]))
        );
        assert_eq!(List::from(Sexpr::True), List::from(vec![Sexpr::True]));
    }

    #[test]
    fn from_iterator() {
        use crate::errors::Error;

        let simple: Vec<Sexpr> = vec![Sexpr::Integer(1), Sexpr::Integer(2), Sexpr::Integer(3)];
        assert_eq!(
            simple.iter().cloned().collect::<Sexpr>(),
            Sexpr::from(vec![
                Sexpr::Integer(1),
                Sexpr::Integer(2),
                Sexpr::Integer(3)
            ])
        );

        let passing: Vec<Result<Sexpr, Error<Sexpr>>> = vec![
            Ok(Sexpr::Integer(1)),
            Ok(Sexpr::Integer(2)),
            Ok(Sexpr::Integer(3)),
        ];
        assert_eq!(
            passing
                .iter()
                .cloned()
                .collect::<Result<Sexpr, Error<Sexpr>>>(),
            Ok(Sexpr::from(vec![
                Sexpr::Integer(1),
                Sexpr::Integer(2),
                Sexpr::Integer(3)
            ]))
        );

        let failing: Vec<Result<Sexpr, Error<Sexpr>>> = vec![
            Ok(Sexpr::Integer(1)),
            Ok(Sexpr::Integer(2)),
            Err(Error::from("ok")),
            Ok(Sexpr::Integer(3)),
        ];
        assert_eq!(
            failing
                .iter()
                .cloned()
                .collect::<Result<Sexpr, Error<Sexpr>>>(),
            Err(Error::from("ok"))
        );
    }
}
