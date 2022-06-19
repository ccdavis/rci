use crate::lex::Token;
use crate::types::ObjectCode;

use std::fmt;
#[derive(Clone, Debug, PartialEq)]
pub enum ErrorType {
    Type,
    Parse,
    Execution,
    Evaluation,
    Compiler,
    Internal,
}

impl fmt::Display for ErrorType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        use ErrorType::*;
        let name = match self {
            Type => "Type",
            Parse => "Parse",
            Evaluation => "Evaluation",
            Execution => "Execution",
            Internal => "Internal",
            Compiler => "Compilation",
        };
        write!(f, "{} Error", &name)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Error {
    pub error_type: ErrorType,
    pub ln: usize,
    pub col: usize,
    pub message: String,
}

impl Error {
    pub fn new(t: &Token, error: ErrorType, msg: String) -> Self {
        Self {
            ln: t.line,
            col: t.column,
            error_type: error,
            message: msg,
        }
    }

    pub fn no_location(error: ErrorType, msg: String) -> Self {
        Self {
            ln: 0,
            col: 0,
            error_type: error,
            message: msg,
        }
    }

    pub fn internal(error: ErrorType, msg: String) -> Self {
        Error::no_location(error, msg)
    }

    pub fn format(&self) -> String {
        format!(
            "{} at {}, {}: {}",
            self.error_type, self.ln, self.col, self.message
        )
    }

    // Use the ln and col to add a line of source and a ^ to show the error location.
    pub fn show_with_source(&self, source: String) -> String {
        self.format()
    }
}

// Convenience
pub fn compiler_err(t: &Token, msg: &str) -> Result<ObjectCode,Error> {
    Err(Error::new(t, ErrorType::Compiler, msg.to_string()))
}

// Convenience
pub fn parse_err(t: &Token, msg: &str) -> Error {
    Error::new(t, ErrorType::Parse, msg.to_string())
}
