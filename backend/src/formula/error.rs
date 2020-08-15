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

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorPosition {
    Known(usize),
    End,
}

impl fmt::Display for ErrorPosition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Known(position) => write!(f, "{}", position),
            Self::End => write!(f, "END"),
        }
    }
}

#[derive(Debug, Clone)]
pub struct PositionedFormulaError {
    pub error: FormulaError,
    pub start: ErrorPosition,
    pub end: ErrorPosition,
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
            start: if start == -1 {
                ErrorPosition::End
            } else {
                ErrorPosition::Known(start as usize)
            },
            end: if start == -1 {
                ErrorPosition::End
            } else {
                ErrorPosition::Known(start as usize + 1)
            },
        }
    }
}
