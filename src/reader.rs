use crate::errors::ReadError;
use rustyline::{Config, DefaultEditor};
use std::fs::File;

use std::io::{BufRead, BufReader, Lines};
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

    fn empty() -> StringReader {
        StringReader::from("")
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
    reader: DefaultEditor,
    buffer: StringReader,
}

impl StdinReader {
    pub fn new() -> Result<Self, ReadError> {
        let config = Config::builder().auto_add_history(true).build();
        let reader = match DefaultEditor::with_config(config) {
            Ok(editor) => editor,
            Err(msg) => return Err(ReadError::IoError(msg.to_string())),
        };
        Ok(StdinReader {
            reader,
            buffer: StringReader::empty(),
        })
    }

    fn next_line(&mut self) -> Result<StringReader, ReadError> {
        match self.reader.readline("> ") {
            Ok(line) => Ok(StringReader::from(&format!("{}\n", line))),
            Err(err) => Err(err.into()),
        }
    }
}

impl Reader for StdinReader {
    fn next(&mut self) -> Result<char, ReadError> {
        loop {
            match self.buffer.next() {
                Err(ReadError::EndOfInput) => self.buffer = self.next_line()?,
                result => return result,
            }
        }
    }

    fn peek(&mut self) -> Result<char, ReadError> {
        loop {
            match self.buffer.peek() {
                Err(ReadError::EndOfInput) => self.buffer = self.next_line()?,
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
