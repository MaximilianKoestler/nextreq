use std::fmt;
use std::ops::Deref;

use super::lexer::Token;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(f64),
    Variable(String),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Number(n) => write!(f, "{}", n),
            Self::Variable(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    Neg,
    Sqrt,
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let op = match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Neg => "-",
            Self::Sqrt => "sqrt",
        };
        write!(f, "{}", op)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseItem {
    Value(Value),
    Operator(Operator),
}

impl fmt::Display for ParseItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Value(v) => write!(f, "{}", v),
            Self::Operator(o) => write!(f, "{}", o),
        }
    }
}

pub struct Parser {
    parsed_expression: Vec<ParseItem>,
}

impl Parser {
    pub fn new(tokens: &[Token]) -> Self {
        let parsed_expression = vec![ParseItem::Value(Value::Number(0.0))];
        Parser { parsed_expression }
    }
}

impl Deref for Parser {
    type Target = [ParseItem];

    fn deref(&self) -> &Self::Target {
        &self.parsed_expression
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_value() {
        let tokens = vec![Token::Number(0.0)];
        let parser = Parser::new(&tokens);

        let expected = vec![ParseItem::Value(Value::Number(0.0))];
        assert_eq!(&parser[..], &expected[..]);
    }
}
