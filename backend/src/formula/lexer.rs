use std::fmt;
use std::ops::Deref;
use std::str;

use super::error::FormulaError;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operator {
    Plus,
    Minus,
    Star,
    Slash,
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let op = match self {
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Star => "*",
            Self::Slash => "/",
        };
        write!(f, "{}", op)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Bracket {
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

#[derive(Debug, Clone, PartialEq)]
pub enum Token {
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

pub struct Lexer {
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
            .fold((0, 0), |(count, end), len| (count + 1, end + len));

        // upon returning, `it` must point to the char matching the predicate
        // `it + count` points at the first non-matching or at the end of the string
        // `it + count - 1` points at the last matching (where we need to go)
        if count > 1 {
            self.nth(count - 1 - 1); // -1 because nth(x) advances x + 1 times
        }

        &str_slice[..end]
    }
}

impl Lexer {
    pub fn new(input: &str) -> Result<Lexer, FormulaError> {
        let mut tokens = Vec::new();

        let mut it = input.chars();
        while let Some(c) = it.clone().next() {
            match c {
                '0'..='9' => {
                    let value = Self::get_number(&mut it)?;
                    tokens.push(Token::Number(value));
                }
                x if x.is_alphabetic() => {
                    let value = Self::get_identifier(&mut it);
                    tokens.push(Token::Identifier(value.to_owned()));
                }
                '+' => {
                    tokens.push(Token::Operator(Operator::Plus));
                }
                '-' => {
                    tokens.push(Token::Operator(Operator::Minus));
                }
                '*' => {
                    tokens.push(Token::Operator(Operator::Star));
                }
                '/' => {
                    tokens.push(Token::Operator(Operator::Slash));
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
                        "unexpected symbol: {}",
                        c
                    )));
                }
            };
            it.next();
        }

        Ok(Lexer { tokens })
    }

    fn get_number(it: &mut str::Chars) -> Result<f64, FormulaError> {
        it.take_prefix(|c| c.is_ascii_digit() || *c == '.')
            .parse()
            .map_err(|err: std::num::ParseFloatError| FormulaError::LexerError(err.to_string()))
    }

    fn get_identifier<'a>(it: &'a mut str::Chars) -> &'a str {
        it.take_prefix(|c| c.is_alphanumeric())
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
    use itertools::Itertools;
    use proptest::prelude::*;
    use std::iter::once;

    fn identifier_strategy() -> BoxedStrategy<String> {
        proptest::string::string_regex(r"\p{Alphabetic}[\p{Alphabetic}\d]{0,32}")
            .unwrap()
            .boxed()
    }

    impl Arbitrary for Operator {
        type Parameters = ();
        type Strategy = BoxedStrategy<Self>;

        fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
            prop_oneof![
                Just(Self::Plus),
                Just(Self::Minus),
                Just(Self::Star),
                Just(Self::Slash),
            ]
            .boxed()
        }
    }

    impl Arbitrary for Bracket {
        type Parameters = ();
        type Strategy = BoxedStrategy<Self>;

        fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
            prop_oneof![Just(Self::RoundOpen), Just(Self::RoundClose),].boxed()
        }
    }

    impl Arbitrary for Token {
        type Parameters = ();
        type Strategy = BoxedStrategy<Self>;

        fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
            prop_oneof![
                any::<f64>().prop_map(Self::Number),
                identifier_strategy().prop_map(Self::Identifier),
                any::<Operator>().prop_map(Self::Operator),
                any::<Bracket>().prop_map(Self::Bracket),
            ]
            .boxed()
        }
    }

    fn whitespace_strategy() -> BoxedStrategy<String> {
        proptest::string::string_regex(r"\s{0,16}").unwrap().boxed()
    }

    fn whitespace_for_token_strategy(token: &Token) -> BoxedStrategy<String> {
        // for the random testing, numbers and identifiers must be followed by a space to
        // distinguish them from another number/identifier which could potentially follow directly
        proptest::string::string_regex(match token {
            Token::Number(_) | Token::Identifier(_) => r"\s{1,16}",
            _ => r"\s{0,16}",
        })
        .unwrap()
        .boxed()
    }

    prop_compose! {
        fn token_and_space()
                          (token in any::<Token>())
                          (token in Just(token.clone()),
                                space in whitespace_for_token_strategy(&token))
                          -> (Token, String) {
           (token, space)
       }
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

            let mut expected = vec![Token::Number(value.abs())];
            if value < 0.0 {
                expected.insert(0, Token::Operator(Operator::Minus))
            }
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
        fn tokenize_simple_expression(
            lhs: f64,
            rhs: f64,
            op: Operator,
            spaces in whitespace_strategy(),
        ) {
            let lexer = Lexer::new(&format!("{}{sp}{}{sp}{}", lhs, op, rhs, sp = spaces)).unwrap();
            let mut expected = vec![
                Token::Number(lhs.abs()),
                Token::Operator(op),
                Token::Number(rhs.abs()),
            ];
            if rhs < 0.0 {
                expected.insert(2, Token::Operator(Operator::Minus))
            }
            if lhs < 0.0 {
                expected.insert(0, Token::Operator(Operator::Minus))
            }

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

    #[test]
    fn tokenize_complex_expression() {
        let lexer = Lexer::new("0+ fn3(1.5 + 10* -200)/3/你好").unwrap();
        let expected = vec![
            Token::Number(0.0),
            Token::Operator(Operator::Plus),
            Token::Identifier("fn3".to_owned()),
            Token::Bracket(Bracket::RoundOpen),
            Token::Number(1.5),
            Token::Operator(Operator::Plus),
            Token::Number(10.0),
            Token::Operator(Operator::Star),
            Token::Operator(Operator::Minus),
            Token::Number(200.0),
            Token::Bracket(Bracket::RoundClose),
            Token::Operator(Operator::Slash),
            Token::Number(3.0),
            Token::Operator(Operator::Slash),
            Token::Identifier("你好".to_owned()),
        ];
        assert_eq!(&lexer[..], &expected[..]);
    }

    proptest! {
        #![proptest_config(ProptestConfig {
            cases: 32,
            max_shrink_iters: 2048,
            .. ProptestConfig::default()
        })]
        #[test]
        fn tokenize_random_expression(input in prop::collection::vec(token_and_space(), 0..64)) {
            let token_str = input
                .iter()
                .flat_map(|(token, space)| once(token.to_string()).chain(once(space.clone())))
                .join("");

            let expected: Vec<_> = input
                .into_iter()
                .map(|(token, _)| token)
                .flat_map(|token| match token {
                    Token::Number(value) if value < 0.0 => {
                        vec![Token::Operator(Operator::Minus), Token::Number(value.abs())]
                    }
                    _ => vec![token],
                })
                .collect();

            let lexer = Lexer::new(&token_str).unwrap();
            assert_eq!(&lexer[..], &expected[..]);
        }
    }
}
