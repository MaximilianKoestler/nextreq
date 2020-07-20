use std::fmt;
use std::ops::Deref;

use super::error::FormulaError;
use super::lexer::{Operator as LexerOperator, Token};

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

impl From<&LexerOperator> for Operator {
    fn from(op: &LexerOperator) -> Self {
        match op {
            LexerOperator::Add => Self::Add,
            LexerOperator::Sub => Self::Sub,
            LexerOperator::Mul => Self::Mul,
            LexerOperator::Div => Self::Div,
        }
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

impl From<&LexerOperator> for ParseItem {
    fn from(op: &LexerOperator) -> Self {
        Self::Operator(op.into())
    }
}

pub struct Parser {
    parsed_expression: Vec<ParseItem>,
}

type PeekableToken<'a> = std::iter::Peekable<std::slice::Iter<'a, Token>>;

impl Parser {
    pub fn new(tokens: &[Token]) -> Result<Self, FormulaError> {
        let parsed_expression = Self::expression(&mut tokens.iter().peekable(), 0)?;

        Ok(Parser { parsed_expression })
    }

    fn expression(it: &mut PeekableToken, min_bp: u8) -> Result<Vec<ParseItem>, FormulaError> {
        let mut result = vec![];

        let lhs = match it.next() {
            Some(Token::Number(value)) => ParseItem::Value(Value::Number(*value)),
            Some(token) => {
                return Err(FormulaError::ParserError(format!(
                    "unsupported token: {}",
                    token
                )))
            }
            None => {
                return Err(FormulaError::ParserError(
                    "unexpected end of expression".to_owned(),
                ))
            }
        };
        result.push(lhs);

        loop {
            let op = match it.peek() {
                None => break,
                Some(Token::Operator(op)) => op,
                t => panic!("bad token: {:?}", t),
            };
            let (l_bp, r_bp) = Self::infix_binding_power(op);

            if l_bp < min_bp {
                break;
            }

            it.next();
            let rhs = Self::expression(it, r_bp)?;
            result.extend(rhs);
            result.push(op.into());
        }

        Ok(result)
    }

    fn infix_binding_power(op: &LexerOperator) -> (u8, u8) {
        match op {
            LexerOperator::Add | LexerOperator::Sub => (1, 2),
            LexerOperator::Mul | LexerOperator::Div => (3, 4),
        }
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

    #[test]
    fn tokenize_invalid() {
        assert!(Parser::new(&vec![Token::Operator(LexerOperator::Add)]).is_err());
    }

    proptest! {
        #[test]
        fn parse_value(value: f64) {
            let tokens = vec![Token::Number(value)];
            let parser = Parser::new(&tokens).unwrap();
            prop_assert_eq!(parser.test_str(), value.to_string());
        }
    }

    proptest! {
        #[test]
        fn parse_simple_expression(lhs: f64, rhs: f64, op: LexerOperator) {
            let expected = format!("{} {} {}", lhs, rhs, op);

            let tokens = vec![
                Token::Number(lhs),
                Token::Operator(op),
                Token::Number(rhs),
            ];

            let parser = Parser::new(&tokens).unwrap();
            assert_eq!(parser.test_str(), expected);
        }
    }

    #[test]
    fn parse_complex_expressions() {
        let parser = Parser::new(&vec![
            Token::Number(1.0),
            Token::Operator(LexerOperator::Add),
            Token::Number(2.0),
            Token::Operator(LexerOperator::Sub),
            Token::Number(3.0),
        ])
        .unwrap();
        assert_eq!(parser.test_str(), format!("1 2 + 3 -"));

        let parser = Parser::new(&vec![
            Token::Number(1.0),
            Token::Operator(LexerOperator::Add),
            Token::Number(2.0),
            Token::Operator(LexerOperator::Mul),
            Token::Number(3.0),
        ])
        .unwrap();
        assert_eq!(parser.test_str(), format!("1 2 3 * +"));

        let parser = Parser::new(&vec![
            Token::Number(1.0),
            Token::Operator(LexerOperator::Mul),
            Token::Number(2.0),
            Token::Operator(LexerOperator::Sub),
            Token::Number(3.0),
        ])
        .unwrap();
        assert_eq!(parser.test_str(), format!("1 2 * 3 -"));
    }
}
