use std::fmt;

#[derive(Debug, Clone)]
pub enum FormulaError {
    LexerError(String),
    ParserError(String),
    EvaluationError(String),
    NumericError(String),
}

impl fmt::Display for FormulaError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::LexerError(msg) => write!(f, "Lexing Error: {}", msg),
            Self::ParserError(msg) => write!(f, "Parsing Error: {}", msg),
            Self::EvaluationError(msg) => write!(f, "Evaluation Error: {}", msg),
            Self::NumericError(msg) => write!(f, "Numeric Error: {}", msg),
        }
    }
}
