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
    Offset(usize),
    End,
}

impl fmt::Display for ErrorPosition {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Offset(position) => write!(f, "{}", position),
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

pub trait ErrorMarker {
    fn start(&self) -> usize;
    fn end(&self) -> usize;
}

impl FormulaError {
    pub fn between(self, start: usize, end: usize) -> PositionedFormulaError {
        PositionedFormulaError {
            error: self,
            start: ErrorPosition::Offset(start),
            end: ErrorPosition::Offset(end),
        }
    }

    pub fn to_end(self, start: usize) -> PositionedFormulaError {
        PositionedFormulaError {
            error: self,
            start: ErrorPosition::Offset(start),
            end: ErrorPosition::End,
        }
    }

    pub fn at_end(self) -> PositionedFormulaError {
        PositionedFormulaError {
            error: self,
            start: ErrorPosition::End,
            end: ErrorPosition::End,
        }
    }

    pub fn at_marker(self, marker: &dyn ErrorMarker) -> PositionedFormulaError {
        PositionedFormulaError {
            error: self,
            start: ErrorPosition::Offset(marker.start()),
            end: ErrorPosition::Offset(marker.end()),
        }
    }
}
