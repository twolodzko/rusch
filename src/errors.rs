use rustyline::error::ReadlineError;
use std::fmt;

#[derive(PartialEq, Debug, Clone)]
pub enum Error<T> {
    NotANumber(T),
    NotCallable(T),
    NotFound(String),
    WrongArg(T),
    WrongArgNum,
    CannotParse(T),
    Custom(String),
    ReadError(ReadError),
    Undefined,
}

impl<T> Error<T> {
    pub fn from(msg: &str) -> Self {
        Error::Custom(String::from(msg))
    }
}

impl<T> fmt::Display for Error<T>
where
    T: fmt::Display,
{
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use Error::*;
        match self {
            NotFound(str) => write!(f, "{} was not found", str),
            NotCallable(obj) => write!(f, "{} is not callable", obj),
            WrongArgNum => write!(f, "incorrect number of arguments"),
            WrongArg(obj) => write!(f, "invalid argument {}", obj),
            NotANumber(obj) => write!(f, "{} is not a number", obj),
            CannotParse(obj) => write!(f, "{} could not be parsed", obj),
            Custom(msg) => msg.fmt(f),
            ReadError(msg) => msg.fmt(f),
            Undefined => write!(f, "undefined result"),
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum ReadError {
    EndOfInput,
    Interrupted,
    Unexpected(char),
    Missing(String),
    IoError(String),
}

impl fmt::Display for ReadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ReadError::*;
        match self {
            Interrupted => write!(f, "interrupted"),
            EndOfInput => write!(f, "end of input"),
            Unexpected(ch) => write!(f, "unexpected character '{}'", ch),
            Missing(msg) => write!(f, "missing {}", msg),
            IoError(msg) => msg.fmt(f),
        }
    }
}

impl From<ReadlineError> for ReadError {
    fn from(value: ReadlineError) -> Self {
        match value {
            ReadlineError::Eof => ReadError::EndOfInput,
            ReadlineError::Interrupted => ReadError::Interrupted,
            other => ReadError::IoError(other.to_string()),
        }
    }
}
