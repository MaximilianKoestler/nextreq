use std::fmt;
use std::iter::Peekable;
use std::ops::Deref;

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

fn get_number<T: Iterator<Item = char>>(iter: &mut Peekable<T>) -> f64 {
    let mut number = 0;

    while let Some(Ok(digit)) = iter.peek().map(|c| c.to_string().parse::<u64>()) {
        number = number * 10 + digit;
        iter.next();
    }

    number as f64
}

impl Lexer {
    fn new(input: &str) -> Result<Lexer, String> {
        let mut tokens = Vec::new();

        let mut it = input.chars().peekable();
        while let Some(&c) = it.peek() {
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
        fn tokenize_value(value: u32) {
            let lexer = Lexer::new(&value.to_string()).unwrap();

            let expected = vec![Token::Number(value as f64)];
            prop_assert_eq!(&lexer[..], &expected[..]);
        }
    }

    proptest! {
        #[test]
        fn tokenize_simple_expression(lhs: u32, rhs: u32, op in operator_strategy()) {
            let lexer = Lexer::new(&format!("{} {} {}", lhs, op, rhs)).unwrap();

            let expected = vec![
                Token::Number(lhs as f64),
                Token::Operator(op),
                Token::Number(rhs as f64),
            ];
            prop_assert_eq!(&lexer[..], &expected[..]);
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
}
