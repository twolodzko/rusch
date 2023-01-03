use std::fmt;

#[derive(PartialEq, Debug, Clone)]
pub enum Error<T> {
    NotANumber(T),
    NotCallable(T),
    NotFound(std::string::String),
    WrongArg(T),
    WrongArgNum,
    CannotParse(T),
    Custom(std::string::String),
    ReadError(std::string::String),
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
        }
    }
}

#[derive(PartialEq, Debug)]
pub enum ReadError {
    EndOfInput,
    Unexpected(char),
    Missing(char),
    IoError(std::string::String),
}

impl fmt::Display for ReadError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ReadError::*;
        match self {
            EndOfInput => write!(f, "end of input"),
            Unexpected(ch) => write!(f, "unexpected character '{}'", ch),
            Missing(ch) => write!(f, "missing '{}'", ch),
            IoError(msg) => msg.fmt(f),
        }
    }
}
