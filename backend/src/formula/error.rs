use std::fmt;

#[derive(Debug, Clone)]
pub enum FormulaError {
    LexerError(String),
    ParserError(String),
}

impl fmt::Display for FormulaError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FormulaError::LexerError(msg) => write!(f, "Lexing Error: {}", msg),
            FormulaError::ParserError(msg) => write!(f, "Parsing Error: {}", msg),
        }
    }
}
