use std::fmt;
use std::iter::Peekable;
use std::ops::Deref;
use std::str;

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

fn get_number(it: &mut str::Chars) -> f64 {
    let str_slice = it.as_str();

    let numeric_chars = it
        .clone()
        .take_while(|c| c.is_ascii_digit() || *c == '.')
        .count();

    // upon returning, `it` must point to the last numeric char
    // `it + numeric_chars` points at the first non-numeric or at the end of the string
    // `it + numeric_chars - 1` points at the last numeric char (where we need to go)
    if numeric_chars > 1 {
        it.nth(numeric_chars - 1 - 1); // -1 because nth(x) advances x + 1 times
    }

    str_slice[..numeric_chars].parse().unwrap()
}

impl Lexer {
    fn new(input: &str) -> Result<Lexer, String> {
        let mut tokens = Vec::new();

        let mut it = input.chars();
        while let Some(c) = it.clone().next() {
            match c {
                '0'..='9' => {
                    let value = get_number(&mut it);
                    tokens.push(Token::Number(value));
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
                ' ' => {}
                _ => {
                    return Err(format!("Unexpected symbol: {}", c));
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

    #[test]
    fn tokenize_invalid() {
        let lexer = Lexer::new("$");
        assert!(lexer.is_err());
    }

    proptest! {
        #[test]
        fn tokenize_value(value: f64) {
            let lexer = Lexer::new(&value.to_string()).unwrap();

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
        let lexer = Lexer::new("0+ (1.5 + 10* -200)/3").unwrap();
        let expected = vec![
            Token::Number(0.0),
            Token::Operator(Operator::Add),
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
        ];
        assert_eq!(&lexer[..], &expected[..]);
    }
}
