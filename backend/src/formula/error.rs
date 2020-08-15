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

#[derive(Debug, Clone)]
pub struct PositionedFormulaError {
    pub error: FormulaError,
    pub start: isize,
    pub end: isize,
}

impl fmt::Display for PositionedFormulaError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{} (originating at {}..{})",
            self.error, self.start, self.end
        )
    }
}

impl FormulaError {
    pub fn at(self, start: isize) -> PositionedFormulaError {
        PositionedFormulaError {
            error: self,
            start,
            end: if start == -1 { -1 } else { start + 1 },
        }
    }
}
