use crate::errors::ReadError;
use crate::list::List;
use crate::reader::Reader;
use crate::types::Sexpr;

/// Read a single S-expression from a string
pub fn read_sexpr(r: &mut impl Reader) -> Result<Sexpr, ReadError> {
    skip_whitespace(r)?;
    let c = r.peek()?;
    match c {
        '(' => {
            r.next()?;
            read_list(r)
        }
        ')' => {
            r.next()?; // needed so that REPL does not go into infinite loop
            Err(ReadError::Unexpected(c))
        }
        '\'' => {
            r.next()?;
            read_quote(r)
        }
        '"' => {
            r.next()?;
            read_string(r)
        }
        ';' => {
            skip_line(r)?;
            read_sexpr(r)
        }
        ',' => unimplemented!(),
        _ => {
            let word = read_word(r)?;
            Ok(parse_word(word))
        }
    }
}

fn parse_word(word: String) -> Sexpr {
    match word.as_str() {
        "#t" => Sexpr::True,
        "#f" => Sexpr::False,
        _ => number_or_symbol(word),
    }
}

fn number_or_symbol(word: String) -> Sexpr {
    if let Ok(num) = word.parse::<i64>() {
        Sexpr::Integer(num)
    } else if let Ok(num) = word.parse::<f64>() {
        Sexpr::Float(num)
    } else {
        Sexpr::Symbol(word)
    }
}

fn read_word(r: &mut impl Reader) -> Result<String, ReadError> {
    let mut chars: Vec<char> = vec![];
    loop {
        match r.peek() {
            Ok(c) => {
                if is_word_boundary(c) {
                    break;
                }
            }
            Err(ReadError::EndOfInput) => break,
            Err(msg) => return Err(msg),
        }

        if let Ok(c) = r.next() {
            chars.push(c);
        }
    }
    Ok(chars.into_iter().collect())
}

#[inline]
fn is_word_boundary(c: char) -> bool {
    c.is_whitespace() || matches!(c, '(' | ')' | '\'' | ',' | '"' | ';')
}

fn read_string(r: &mut impl Reader) -> Result<Sexpr, ReadError> {
    let mut chars: Vec<char> = vec![];
    loop {
        match r.next() {
            Err(ReadError::EndOfInput) => return Err(ReadError::Missing('"')),
            Ok('"') => break,
            Ok('\\') => todo!(),
            Ok(c) => chars.push(c),
            Err(msg) => return Err(msg),
        }
    }
    let string = chars.into_iter().collect();
    Ok(Sexpr::String(string))
}

fn read_list(r: &mut impl Reader) -> Result<Sexpr, ReadError> {
    let mut list = List::empty();
    loop {
        skip_whitespace(r)?;

        match r.peek() {
            Ok(')') => {
                r.next()?;
                break;
            }
            Ok(_) => (),
            Err(ReadError::EndOfInput) => return Err(ReadError::Missing(')')),
            Err(msg) => return Err(msg),
        }

        let sexpr = read_sexpr(r)?;
        list = list.push_front(sexpr);
    }
    Ok(Sexpr::List(list.rev()))
}

fn read_quote(r: &mut impl Reader) -> Result<Sexpr, ReadError> {
    let mut list = List::empty();
    let obj = read_sexpr(r)?;
    list = list.push_front(obj);
    list = list.push_front(Sexpr::symbol("quote"));
    Ok(Sexpr::List(list))
}

#[inline]
fn skip_whitespace(r: &mut impl Reader) -> Result<(), ReadError> {
    loop {
        match r.peek() {
            Ok(c) => {
                if !c.is_whitespace() {
                    break;
                }
                r.next()?;
            }
            Err(ReadError::EndOfInput) => break,
            Err(msg) => return Err(msg),
        }
    }
    Ok(())
}

#[inline]
fn skip_line(r: &mut impl Reader) -> Result<(), ReadError> {
    loop {
        match r.next() {
            Ok(c) => {
                if c == '\n' {
                    break;
                }
            }
            Err(ReadError::EndOfInput) => break,
            Err(msg) => return Err(msg),
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use crate::parser::{read_sexpr, ReadError};
    use crate::reader::{Reader, StringReader};
    use crate::types::Sexpr;

    #[test]
    fn empty() {
        let mut r = StringReader::from("");
        assert_eq!(read_sexpr(&mut r), Err(ReadError::EndOfInput));
    }

    #[test]
    fn space() {
        let mut r = StringReader::from("hello world!");
        assert_eq!(read_sexpr(&mut r), Ok(Sexpr::symbol("hello")));
    }

    #[test]
    fn bracket() {
        let mut r = StringReader::from("hello)world!");
        assert_eq!(read_sexpr(&mut r), Ok(Sexpr::symbol("hello")));
    }

    #[test]
    fn comment() {
        let mut r = StringReader::from("hello;world!");
        assert_eq!(read_sexpr(&mut r), Ok(Sexpr::symbol("hello")));
    }

    #[test]
    fn hashtrue() {
        let mut r = StringReader::from("#t;comment");
        assert_eq!(read_sexpr(&mut r), Ok(Sexpr::True));
    }

    #[test]
    fn hashfalse() {
        let mut r = StringReader::from("#f(1 2 3)");
        assert_eq!(read_sexpr(&mut r), Ok(Sexpr::False));
    }

    #[test]
    fn positive_integer() {
        let mut r = StringReader::from("42(1 2 3)");
        assert_eq!(read_sexpr(&mut r), Ok(Sexpr::Integer(42)));
    }

    #[test]
    fn negative_integer() {
        let mut r = StringReader::from("-1 ;negative number");
        assert_eq!(read_sexpr(&mut r), Ok(Sexpr::Integer(-1)));
    }

    #[test]
    fn float() {
        let mut r = StringReader::from("3.14 ;negative number");
        assert_eq!(read_sexpr(&mut r), Ok(Sexpr::Float(3.14)));
    }

    #[test]
    fn scientific_notation() {
        let mut r = StringReader::from("-5e-6 ;negative number");
        assert_eq!(read_sexpr(&mut r), Ok(Sexpr::Float(-5e-6)));
    }

    #[test]
    fn empty_string() {
        let mut r = StringReader::from("\"\" foo bar");
        assert_eq!(read_sexpr(&mut r), Ok(Sexpr::from("")));
    }

    #[test]
    fn simple_string() {
        let mut r = StringReader::from("\"hello world\" foo bar");
        assert_eq!(read_sexpr(&mut r), Ok(Sexpr::from("hello world")));
    }

    #[test]
    fn quote() {
        let mut r = StringReader::from("'foo");
        assert_eq!(
            read_sexpr(&mut r),
            Ok(Sexpr::from(vec![
                Sexpr::symbol("quote"),
                Sexpr::symbol("foo")
            ]))
        );
    }

    #[test]
    fn empty_list() {
        let mut r = StringReader::from("()");
        assert_eq!(read_sexpr(&mut r), Ok(Sexpr::null()));
    }

    #[test]
    fn simple_list() {
        let mut r = StringReader::from("(1 2 3)");
        assert_eq!(
            read_sexpr(&mut r),
            Ok(Sexpr::from(vec![
                Sexpr::Integer(1),
                Sexpr::Integer(2),
                Sexpr::Integer(3),
            ]))
        );
    }

    #[test]
    fn list_of_lists() {
        let mut r = StringReader::from("((1) (1 2) (1 2 3))");
        assert_eq!(
            read_sexpr(&mut r),
            Ok(Sexpr::from(vec![
                Sexpr::from(vec![Sexpr::Integer(1)]),
                Sexpr::from(vec![Sexpr::Integer(1), Sexpr::Integer(2)]),
                Sexpr::from(vec![
                    Sexpr::Integer(1),
                    Sexpr::Integer(2),
                    Sexpr::Integer(3),
                ])
            ]))
        );
    }

    #[test]
    fn unclosed_list() {
        let mut r = StringReader::from("(1 2 3");
        assert_eq!(read_sexpr(&mut r), Err(ReadError::Missing(')')));
    }

    #[test]
    fn closing_bracket() {
        let mut r = StringReader::from(") 1 2 3");
        assert_eq!(read_sexpr(&mut r), Err(ReadError::Unexpected(')')));
    }

    #[test]
    fn leading_whitespace() {
        let mut r = StringReader::from("   \t\n\t hello world!");
        assert_eq!(read_sexpr(&mut r), Ok(Sexpr::symbol("hello")));
    }

    #[test]
    fn skip_comment() {
        let mut r = StringReader::from("\n\n  ;;comment\n  hello world!");
        assert_eq!(read_sexpr(&mut r), Ok(Sexpr::symbol("hello")));
    }

    #[test]
    fn read_twice() {
        let mut r = StringReader::from("hello world!");
        assert_eq!(read_sexpr(&mut r), Ok(Sexpr::symbol("hello")));
        assert_eq!(r.peek(), Ok(' '));
        assert_eq!(read_sexpr(&mut r), Ok(Sexpr::symbol("world!")));
    }
}
