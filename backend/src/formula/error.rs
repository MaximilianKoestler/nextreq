use std::fmt;

#[derive(Debug, Clone)]
pub enum FormulaError {
    LexerError(String),
}

impl fmt::Display for FormulaError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            FormulaError::LexerError(msg) => write!(f, "{}", msg),
        }
    }
}
