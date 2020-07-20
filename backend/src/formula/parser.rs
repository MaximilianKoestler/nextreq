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
        let parsed_expression = Self::expression(&mut tokens.iter());

        Parser { parsed_expression }
    }

    fn expression(it: &mut std::slice::Iter<'_, Token>) -> Vec<ParseItem> {
        let lhs = match it.next() {
            Some(Token::Number(value)) => ParseItem::Value(Value::Number(*value)),
            t => panic!("Unsupported token: {:?}", t),
        };
        vec![lhs]
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
    use itertools::Itertools;
    use proptest::prelude::*;

    impl Parser {
        fn test_str(&self) -> String {
            self.iter().map(|p| p.to_string()).join(" ")
        }
    }

    proptest! {
        #[test]
        fn parse_value(value: f64) {
            let tokens = vec![Token::Number(value)];
            let parser = Parser::new(&tokens);
            prop_assert_eq!(parser.test_str(), value.to_string());
        }
    }
}
