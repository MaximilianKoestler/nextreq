use std::fmt;
use std::iter::Peekable;
use std::ops::Deref;
use std::str;

use super::error::FormulaError;

#[derive(Debug, Clone, PartialEq, Eq)]
enum Operator {
    Add,
    Sub,
    Mul,
    Div,
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let op = match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
        };
        write!(f, "{}", op)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum Bracket {
    RoundOpen,
    RoundClose,
}

impl fmt::Display for Bracket {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let bracket = match self {
            Self::RoundOpen => "(",
            Self::RoundClose => ")",
        };
        write!(f, "{}", bracket)
    }
}

#[derive(Debug, PartialEq)]
enum Token {
    Number(f64),
    Identifier(String),
    Operator(Operator),
    Bracket(Bracket),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Number(n) => write!(f, "{}", n),
            Self::Identifier(s) => write!(f, "{}", s),
            Self::Operator(o) => write!(f, "{}", o),
            Self::Bracket(b) => write!(f, "{}", b),
        }
    }
}

struct Lexer {
    tokens: Vec<Token>,
}

trait PrefixTakable<'a> {
    fn take_prefix<P>(self: &mut Self, predicate: P) -> &'a str
    where
        P: Fn(&char) -> bool;
}

impl<'a> PrefixTakable<'a> for str::Chars<'a> {
    fn take_prefix<P>(self: &mut Self, predicate: P) -> &'a str
    where
        P: Fn(&char) -> bool,
    {
        // This function is O(n), but takes 3 passes over the matching part of the iterator
        // 1st pass: find the end of the matching prefix
        // 2nd pass: forward the iterator to the last character of the prefix
        // 3rd pass: extract the string slice (indexing utf8-slices is O(n))

        let str_slice = self.as_str();

        let (count, end) = self
            .clone()
            .take_while(predicate)
            .map(|c| c.len_utf8())
            .fold((0, 0), |acc, x| (acc.0 + 1, acc.1 + x));

        // upon returning, `it` must point to the char matching the predicate
        // `it + count` points at the first non-matching or at the end of the string
        // `it + count - 1` points at the last matching (where we need to go)
        if count > 1 {
            self.nth(count - 1 - 1); // -1 because nth(x) advances x + 1 times
        }

        &str_slice[..end]
    }
}

fn get_number(it: &mut str::Chars) -> Result<f64, FormulaError> {
    it.take_prefix(|c| c.is_ascii_digit() || *c == '.')
        .parse()
        .map_err(|err: std::num::ParseFloatError| FormulaError::LexerError(err.to_string()))
}

fn get_identifier<'a>(it: &'a mut str::Chars) -> &'a str {
    it.take_prefix(|c| c.is_alphanumeric())
}

impl Lexer {
    fn new(input: &str) -> Result<Lexer, FormulaError> {
        let mut tokens = Vec::new();

        let mut it = input.chars();
        while let Some(c) = it.clone().next() {
            match c {
                '0'..='9' => {
                    let value = get_number(&mut it)?;
                    tokens.push(Token::Number(value));
                }
                x if x.is_alphabetic() => {
                    let value = get_identifier(&mut it);
                    tokens.push(Token::Identifier(value.to_owned()));
                }
                '+' => {
                    tokens.push(Token::Operator(Operator::Add));
                }
                '-' => {
                    tokens.push(Token::Operator(Operator::Sub));
                }
                '*' => {
                    tokens.push(Token::Operator(Operator::Mul));
                }
                '/' => {
                    tokens.push(Token::Operator(Operator::Div));
                }
                '(' => {
                    tokens.push(Token::Bracket(Bracket::RoundOpen));
                }
                ')' => {
                    tokens.push(Token::Bracket(Bracket::RoundClose));
                }
                x if x.is_whitespace() => {}
                _ => {
                    return Err(FormulaError::LexerError(format!(
                        "Unexpected symbol: {}",
                        c
                    )));
                }
            };
            it.next();
        }

        Ok(Lexer { tokens })
    }
}

impl Deref for Lexer {
    type Target = [Token];

    fn deref(&self) -> &Self::Target {
        &self.tokens
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    fn operator_strategy() -> BoxedStrategy<Operator> {
        prop_oneof![
            Just(Operator::Add),
            Just(Operator::Sub),
            Just(Operator::Mul),
            Just(Operator::Div),
        ]
        .boxed()
    }

    fn identifier_strategy() -> BoxedStrategy<String> {
        proptest::string::string_regex(r"\p{Alphabetic}[\p{Alphabetic}\d]*")
            .unwrap()
            .boxed()
    }

    fn whitespace_strategy() -> BoxedStrategy<String> {
        proptest::string::string_regex(r"\s*").unwrap().boxed()
    }

    #[test]
    fn tokenize_invalid() {
        assert!(Lexer::new("$").is_err());
        assert!(Lexer::new("1.0.0").is_err());
    }

    proptest! {
        #[test]
        fn tokenize_value(value: f64, spaces in whitespace_strategy()) {
            let lexer = Lexer::new(&format!("{}{}", value, spaces)).unwrap();

            let mut expected = vec![];
            if value < 0.0 {
                expected.push(Token::Operator(Operator::Sub))
            }
            expected.push(Token::Number(value.abs()));
            prop_assert_eq!(&lexer[..], &expected[..]);
        }
    }

    proptest! {
        #[test]
        fn tokenize_identifier(value in identifier_strategy(), spaces in whitespace_strategy()) {
            let lexer = Lexer::new(&format!("{}{}", value, spaces)).unwrap();
            let expected = vec![Token::Identifier(value)];
            assert_eq!(&lexer[..], &expected[..]);
        }
    }

    proptest! {
        #[test]
        fn tokenize_simple_expression(lhs: f64, rhs: f64, op in operator_strategy()) {
            let lexer_no_spaces = Lexer::new(&format!("{}{}{}", lhs, op, rhs)).unwrap();
            let lexer_with_spaces = Lexer::new(&format!("{} {} {}", lhs, op, rhs)).unwrap();

            let mut expected = vec![];
            if lhs < 0.0 {
                expected.push(Token::Operator(Operator::Sub))
            }
            expected.push(Token::Number(lhs.abs()));
            expected.push(Token::Operator(op));
            if rhs < 0.0 {
                expected.push(Token::Operator(Operator::Sub))
            }
            expected.push(Token::Number(rhs.abs()));

            prop_assert_eq!(&lexer_no_spaces[..], &expected[..]);
            prop_assert_eq!(&lexer_with_spaces[..], &expected[..]);
        }
    }

    #[test]
    fn tokenize_bracket_pair() {
        let lexer = Lexer::new("()").unwrap();
        let expected = vec![
            Token::Bracket(Bracket::RoundOpen),
            Token::Bracket(Bracket::RoundClose),
        ];
        assert_eq!(&lexer[..], &expected[..]);
    }

    #[test]
    fn tokenize_complex_expression() {
        let lexer = Lexer::new("0+ fn3(1.5 + 10* -200)/3/你好").unwrap();
        let expected = vec![
            Token::Number(0.0),
            Token::Operator(Operator::Add),
            Token::Identifier("fn3".to_owned()),
            Token::Bracket(Bracket::RoundOpen),
            Token::Number(1.5),
            Token::Operator(Operator::Add),
            Token::Number(10.0),
            Token::Operator(Operator::Mul),
            Token::Operator(Operator::Sub),
            Token::Number(200.0),
            Token::Bracket(Bracket::RoundClose),
            Token::Operator(Operator::Div),
            Token::Number(3.0),
            Token::Operator(Operator::Div),
            Token::Identifier("你好".to_owned()),
        ];
        assert_eq!(&lexer[..], &expected[..]);
    }
}
