use crate::errors::ReadError;
use std::fs::File;
use std::io;
use std::io::{BufRead, BufReader, Lines, Write};
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

pub struct FileReader {
    lines: Lines<BufReader<File>>,
    iter: StringReader,
}

impl FileReader {
    pub fn from(filename: &str) -> Result<Self, ReadError> {
        let file = File::open(filename).map_err(|msg| ReadError::IoError(msg.to_string()))?;
        let mut lines = BufReader::new(file).lines();
        let iter = FileReader::next_line(&mut lines)?;
        Ok(FileReader { lines, iter })
    }

    fn next_line(lines: &mut Lines<BufReader<File>>) -> Result<StringReader, ReadError> {
        match lines.next() {
            Some(Ok(line)) => Ok(StringReader::from(&format!("\n{}", line))),
            Some(Err(msg)) => Err(ReadError::IoError(msg.to_string())),
            None => Err(ReadError::EndOfInput),
        }
    }
}

impl Reader for FileReader {
    fn next(&mut self) -> Result<char, ReadError> {
        loop {
            match self.iter.next() {
                Err(ReadError::EndOfInput) => self.iter = FileReader::next_line(&mut self.lines)?,
                result => return result,
            }
        }
    }

    fn peek(&mut self) -> Result<char, ReadError> {
        loop {
            match self.iter.peek() {
                Err(ReadError::EndOfInput) => self.iter = FileReader::next_line(&mut self.lines)?,
                result => return result,
            }
        }
    }
}

pub struct StdinReader {
    iter: StringReader,
}

impl StdinReader {
    pub fn new() -> Result<Self, ReadError> {
        Ok(StdinReader {
            iter: StdinReader::next_line()?,
        })
    }

    fn next_line() -> Result<StringReader, ReadError> {
        print!("> ");
        let _ = io::stdout().flush();

        let mut buffer = String::new();
        match io::stdin().read_line(&mut buffer) {
            Ok(_) => Ok(StringReader::from(&buffer)),
            Err(msg) => Err(ReadError::IoError(msg.to_string())),
        }
    }
}

impl Reader for StdinReader {
    fn next(&mut self) -> Result<char, ReadError> {
        loop {
            match self.iter.next() {
                Err(ReadError::EndOfInput) => self.iter = StdinReader::next_line()?,
                result => return result,
            }
        }
    }

    fn peek(&mut self) -> Result<char, ReadError> {
        loop {
            match self.iter.peek() {
                Err(ReadError::EndOfInput) => self.iter = StdinReader::next_line()?,
                result => return result,
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{FileReader, Reader, StringReader};
    use crate::errors::ReadError;

    #[test]
    fn string_reader() {
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

    #[test]
    fn file_reader() {
        // FileReader works the same as just iterating over the lines and characters

        use std::fs::File;
        use std::io::{BufRead, BufReader};

        let filename = "src/reader.rs";

        let file = BufReader::new(File::open(&filename).expect("Unable to open file"));
        let chars = &mut Vec::<char>::new();
        for line in file.lines() {
            chars.push('\n');
            for ch in line.expect("Unable to read line").chars() {
                chars.push(ch);
            }
        }

        let reader = &mut FileReader::from(filename).unwrap();
        for ch in chars {
            assert_eq!(Ok(*ch), reader.next());
        }
    }
}
