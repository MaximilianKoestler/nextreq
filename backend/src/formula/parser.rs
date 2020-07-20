use std::fmt;
use std::ops::Deref;

use super::error::{FormulaError, FormulaError::ParserError};
use super::lexer::{Bracket as LexerBracket, Operator as LexerOperator, Token};

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
    Pos,
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
            Self::Pos => "+",
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

        match it.next() {
            Some(Token::Number(value)) => result.push(ParseItem::Value(Value::Number(*value))),
            Some(Token::Operator(op)) => {
                let ((), r_bp) = Self::prefix_binding_power(op);
                let rhs = Self::expression(it, r_bp)?;

                let op = match op {
                    LexerOperator::Add => Operator::Pos,
                    LexerOperator::Sub => Operator::Neg,
                    _ => return Err(ParserError(format!("unsupported prefix operator: {}", op))),
                };
                result.extend(rhs);
                result.push(ParseItem::Operator(op));
            }
            Some(Token::Bracket(LexerBracket::RoundOpen)) => {
                let lhs = Self::expression(it, 0)?;
                assert_eq!(
                    it.next().unwrap(),
                    &Token::Bracket(LexerBracket::RoundClose)
                );
                result.extend(lhs);
            }
            Some(token) => return Err(ParserError(format!("unsupported token: {}", token))),
            None => return Err(ParserError("unexpected end of expression".to_owned())),
        };

        loop {
            let op = match it.peek() {
                None => break,
                Some(Token::Bracket(LexerBracket::RoundClose)) => break,
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

    fn prefix_binding_power(op: &LexerOperator) -> ((), u8) {
        match op {
            LexerOperator::Add | LexerOperator::Sub => ((), 5),
            _ => panic!("bad prefix operator: {:?}", op),
        }
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
    use std::iter::once;

    impl Parser {
        fn postfix(&self) -> String {
            self.iter().map(|p| p.to_string()).join(" ")
        }
    }

    #[derive(Debug)]
    enum TokenTree {
        Number(f64),
        Variable(String),
        Expression(LexerOperator, Vec<TokenTree>),
    }

    impl TokenTree {
        fn postfix(&self) -> String {
            match self {
                TokenTree::Number(value) => value.to_string(),
                TokenTree::Variable(name) => name.to_string(),
                TokenTree::Expression(operator, operands) => operands
                    .iter()
                    .map(TokenTree::postfix)
                    .chain(once(operator.to_string()))
                    .join(" "),
            }
        }

        fn infix(&self) -> Vec<Token> {
            match self {
                TokenTree::Number(value) => vec![Token::Number(*value)],
                TokenTree::Variable(name) => vec![Token::Identifier(name.clone())],
                TokenTree::Expression(operator, operands) => match operands.len() {
                    1 => once(Token::Bracket(LexerBracket::RoundOpen))
                        .chain(once(Token::Operator(operator.clone())))
                        .chain(operands[0..1].iter().flat_map(TokenTree::infix))
                        .chain(once(Token::Bracket(LexerBracket::RoundClose)))
                        .collect(),
                    2 => once(Token::Bracket(LexerBracket::RoundOpen))
                        .chain(operands[0..1].iter().flat_map(TokenTree::infix))
                        .chain(once(Token::Operator(operator.clone())))
                        .chain(operands[1..].iter().flat_map(TokenTree::infix))
                        .chain(once(Token::Bracket(LexerBracket::RoundClose)))
                        .collect(),
                    i => panic!("unsupported number of operands: {}", i),
                },
            }
        }
    }

    fn unary_operator() -> BoxedStrategy<LexerOperator> {
        prop_oneof![Just(LexerOperator::Add), Just(LexerOperator::Sub),].boxed()
    }

    prop_compose! {
        fn unary_expression(base: BoxedStrategy<TokenTree>)
                          (operator in unary_operator(),
                                operands in prop::collection::vec(base, 1))
                          -> TokenTree {
           TokenTree::Expression(operator, operands)
       }
    }

    prop_compose! {
        fn binary_expression(base: BoxedStrategy<TokenTree>)
                          (operator in any::<LexerOperator>(),
                                operands in prop::collection::vec(base, 2))
                          -> TokenTree {
           TokenTree::Expression(operator, operands)
       }
    }

    impl Arbitrary for TokenTree {
        type Parameters = ();
        type Strategy = BoxedStrategy<Self>;

        fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
            let leaf = prop_oneof![
                (0..100u32).prop_map(|v| TokenTree::Number(v as f64)),
                // r"[[:lower:]]{1}".prop_map(TokenTree::Variable),
            ];

            leaf.prop_recursive(8, 64, 2, |inner| {
                prop_oneof![
                    unary_expression(inner.clone()),
                    binary_expression(inner.clone()),
                ]
            })
            .boxed()
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
            prop_assert_eq!(parser.postfix(), value.to_string());
        }
    }

    #[test]
    fn parse_bracket_value() {
        let tokens = vec![
            Token::Bracket(LexerBracket::RoundOpen),
            Token::Number(1.0),
            Token::Bracket(LexerBracket::RoundClose),
        ];
        let parser = Parser::new(&tokens).unwrap();
        assert_eq!(parser.postfix(), "1".to_owned());
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
            assert_eq!(parser.postfix(), expected);
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
        assert_eq!(parser.postfix(), format!("1 2 + 3 -"));

        let parser = Parser::new(&vec![
            Token::Number(1.0),
            Token::Operator(LexerOperator::Add),
            Token::Number(2.0),
            Token::Operator(LexerOperator::Mul),
            Token::Number(3.0),
        ])
        .unwrap();
        assert_eq!(parser.postfix(), format!("1 2 3 * +"));

        let parser = Parser::new(&vec![
            Token::Number(1.0),
            Token::Operator(LexerOperator::Mul),
            Token::Number(2.0),
            Token::Operator(LexerOperator::Sub),
            Token::Number(3.0),
        ])
        .unwrap();
        assert_eq!(parser.postfix(), format!("1 2 * 3 -"));
    }

    proptest! {
        #[test]
        fn arbitrary_expression(token_tree: TokenTree) {
            let parser = Parser::new(&token_tree.infix()).unwrap();
            assert_eq!(parser.postfix(), token_tree.postfix());
        }
    }
}