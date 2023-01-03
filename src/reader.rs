use crate::errors::ReadError;
use std::iter::Peekable;
use std::vec::IntoIter;

pub trait Reader {
    fn peek(&mut self) -> Result<char, ReadError>;
    fn next(&mut self) -> Result<char, ReadError>;
}

pub struct StringReader {
    cache: Peekable<IntoIter<char>>,
}

impl StringReader {
    pub fn from(s: &str) -> StringReader {
        StringReader {
            cache: s.chars().collect::<Vec<_>>().into_iter().peekable(),
        }
    }
}

impl Reader for StringReader {
    #[inline]
    fn peek(&mut self) -> Result<char, ReadError> {
        self.cache.peek().ok_or(ReadError::EndOfInput).cloned()
    }

    #[inline]
    fn next(&mut self) -> Result<char, ReadError> {
        self.cache.next().ok_or(ReadError::EndOfInput)
    }
}

#[cfg(test)]
mod tests {
    use super::StringReader;
    use crate::errors::ReadError;
    use crate::reader::Reader;

    #[test]
    fn stringreader() {
        let mut r = StringReader::from("hello");
        assert_eq!(r.next(), Ok('h'));
        assert_eq!(r.peek(), Ok('e'));
        assert_eq!(r.peek(), Ok('e'));
        assert_eq!(r.next(), Ok('e'));
        assert_eq!(r.next(), Ok('l'));
        assert_eq!(r.next(), Ok('l'));
        assert_eq!(r.next(), Ok('o'));
        assert_eq!(r.next(), Err(ReadError::EndOfInput));
        assert_eq!(r.next(), Err(ReadError::EndOfInput));
        assert_eq!(r.peek(), Err(ReadError::EndOfInput));
    }
}
