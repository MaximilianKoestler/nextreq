use std::fmt;
use std::ops::Deref;
use std::str;

use super::error::{FormulaError, PositionedFormulaError};
use super::numeric::{Numeric, ParseError};
use super::quoted_string::Quotable;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Operator {
    Plus,
    Minus,
    Star,
    Slash,
    Caret,
    ExclamationMark,
    Comma,
}

impl fmt::Display for Operator {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let op = match self {
            Self::Plus => "+",
            Self::Minus => "-",
            Self::Star => "*",
            Self::Slash => "/",
            Self::Caret => "^",
            Self::ExclamationMark => "!",
            Self::Comma => ",",
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
    Number(Numeric),
    Literal(String),
    Identifier(String),
    Operator(Operator),
    Bracket(Bracket),
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Number(n) => write!(f, "{}", n),
            Self::Literal(l) => write!(f, "{}", l.quote()),
            Self::Identifier(s) => write!(f, "{}", s),
            Self::Operator(o) => write!(f, "{}", o),
            Self::Bracket(b) => write!(f, "{}", b),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct PositionedToken {
    pub token: Token,
    pub start: usize,
    pub length: usize,
}

impl Token {
    pub fn at(self, start: usize, length: usize) -> PositionedToken {
        PositionedToken {
            token: self,
            start,
            length,
        }
    }
}

impl fmt::Display for PositionedToken {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.token)
    }
}

#[derive(Debug)]
pub struct Lexer {
    tokens: Vec<PositionedToken>,
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
    pub fn new(input: &str) -> Result<Lexer, PositionedFormulaError> {
        let mut tokens = Vec::new();

        let mut it = input.chars();
        let mut offset = 0;
        while let Some(c) = it.clone().next() {
            match c {
                '0'..='9' => {
                    let (value, length) = Self::get_number(&mut it, offset)?;
                    tokens.push(Token::Number(value).at(offset, length));
                    offset += length - 1;
                }
                '"' => {
                    let (literal, forward) = Self::get_literal(&mut it, offset)?;
                    tokens.push(Token::Literal(literal.to_owned()).at(offset, forward + 1));
                    offset += forward;
                }
                x if x.is_alphabetic() => {
                    let (name, forward) = Self::get_identifier(&mut it);
                    tokens.push(Token::Identifier(name.to_owned()).at(offset, forward + 1));
                    offset += forward;
                }
                '+' => {
                    tokens.push(Token::Operator(Operator::Plus).at(offset, 1));
                }
                '-' => {
                    tokens.push(Token::Operator(Operator::Minus).at(offset, 1));
                }
                '*' => {
                    tokens.push(Token::Operator(Operator::Star).at(offset, 1));
                }
                '/' => {
                    tokens.push(Token::Operator(Operator::Slash).at(offset, 1));
                }
                '^' => {
                    tokens.push(Token::Operator(Operator::Caret).at(offset, 1));
                }
                '!' => {
                    tokens.push(Token::Operator(Operator::ExclamationMark).at(offset, 1));
                }
                ',' => {
                    tokens.push(Token::Operator(Operator::Comma).at(offset, 1));
                }
                '(' => {
                    tokens.push(Token::Bracket(Bracket::RoundOpen).at(offset, 1));
                }
                ')' => {
                    tokens.push(Token::Bracket(Bracket::RoundClose).at(offset, 1));
                }
                x if x.is_whitespace() => {}
                _ => {
                    return Err(
                        FormulaError::LexerError(format!("unexpected symbol: {}", c))
                            .at(offset as isize),
                    )
                }
            };
            it.next();
            offset += 1;
        }

        Ok(Self { tokens })
    }

    fn get_number(
        it: &mut str::Chars,
        offset: usize,
    ) -> Result<(Numeric, usize), PositionedFormulaError> {
        let numeric = it.take_prefix(|c| c.is_ascii_digit() || *c == '.');
        numeric
            .parse()
            .map(|n| (n, numeric.chars().count()))
            .map_err(|err: ParseError| {
                FormulaError::LexerError(err.to_string()).at(offset as isize)
            })
    }

    fn get_literal<'a>(
        it: &'a mut str::Chars,
        offset: usize,
    ) -> Result<(&'a str, usize), PositionedFormulaError> {
        it.next();
        let literal = it.take_prefix(|c| *c != '"');

        let to_check = if literal.is_empty() {
            it.clone().next()
        } else {
            it.next();
            it.clone().next()
        };

        if let Some('"') = to_check {
            Ok((literal, literal.chars().count() + 1))
        } else {
            Err(
                FormulaError::LexerError("literal not terminated by \"".to_owned())
                    .at((offset + literal.chars().count() + 1) as isize),
            )
        }
    }

    fn get_identifier<'a>(it: &'a mut str::Chars) -> (&'a str, usize) {
        let identifier = it.take_prefix(|c| c.is_alphanumeric());
        (identifier, identifier.chars().count() - 1)
    }
}

impl Deref for Lexer {
    type Target = [PositionedToken];

    fn deref(&self) -> &Self::Target {
        &self.tokens
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::*;
    use itertools::Itertools;
    use proptest::prelude::*;
    use std::iter::once;

    fn identifier_strategy() -> BoxedStrategy<String> {
        proptest::string::string_regex(r"\p{Alphabetic}[\p{Alphabetic}\d]{0,32}")
            .unwrap()
            .boxed()
    }

    fn literal_strategy() -> BoxedStrategy<String> {
        proptest::string::string_regex("[^\"]*").unwrap().boxed()
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
                Just(Self::Caret),
                Just(Self::ExclamationMark),
                Just(Self::Comma),
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
                any::<Numeric>().prop_map(Self::Number),
                literal_strategy().prop_map(Self::Literal),
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

    impl Token {
        /// For testing purposes only, convert a single `Token` into a `PositionedToken` at a given
        /// `offset`.
        /// The `offset` is than advanced to point after the processed token.
        ///
        /// This function also handles special cases, e.g. separating the sign character from
        /// negative numbers.
        pub fn expected_sequence(self, offset: &mut usize) -> Vec<PositionedToken> {
            let mut tokens = vec![];
            let mut length = 0;

            match self {
                Token::Number(value) => {
                    length += value.to_string().chars().count();
                    if value < 0.0 {
                        tokens.push(Token::Operator(Operator::Minus).at(*offset, 1));
                        *offset += 1;
                        length -= 1;
                    }
                    tokens.push(Token::Number(value.abs()).at(*offset, length));
                }
                _ => {
                    length += self.to_string().chars().count();
                    tokens.push(self.at(*offset, length));
                }
            }
            *offset += length;
            tokens
        }
    }

    pub trait Positionable {
        fn anywhere(self) -> Vec<PositionedToken>;
        fn at_their_index(self) -> Vec<PositionedToken>;
    }

    impl Positionable for Vec<Token> {
        fn anywhere(self) -> Vec<PositionedToken> {
            self.into_iter().map(|t| t.at(0, 0)).collect()
        }

        fn at_their_index(self) -> Vec<PositionedToken> {
            self.into_iter()
                .enumerate()
                .map(|(i, t)| t.at(i, 1))
                .collect()
        }
    }

    #[test]
    fn tokenize_invalid() {
        use super::super::error::{ErrorPosition::*, FormulaError::LexerError};

        let error = Lexer::new("$").unwrap_err();
        assert!(matches!(error.error, LexerError(_)));
        assert_eq!(error.start, Known(0));

        let error = Lexer::new("123$").unwrap_err();
        assert!(matches!(error.error, LexerError(_)));
        assert_eq!(error.start, Known(3));

        let error = Lexer::new("\"abc\"$").unwrap_err();
        assert!(matches!(error.error, LexerError(_)));
        assert_eq!(error.start, Known(5));

        let error = Lexer::new("abc$").unwrap_err();
        assert!(matches!(error.error, LexerError(_)));
        assert_eq!(error.start, Known(3));

        let error = Lexer::new("1.0.0").unwrap_err();
        assert!(matches!(error.error, LexerError(_)));
        assert_eq!(error.start, Known(0));

        let error = Lexer::new("  2 + 1.0.0").unwrap_err();
        assert!(matches!(error.error, LexerError(_)));
        assert_eq!(error.start, Known(6));

        let error = Lexer::new("\"abc").unwrap_err();
        assert!(matches!(error.error, LexerError(_)));
        assert_eq!(error.start, Known(4));
    }

    proptest! {
        #[test]
        fn tokenize_number(value: Numeric, spaces in whitespace_strategy()) {
            let lexer = Lexer::new(&format!("{sp}{}{sp}", value, sp = spaces)).unwrap();

            let expected = Token::Number(value).expected_sequence(&mut spaces.chars().count());
            prop_assert_eq!(&lexer[..], &expected[..]);
        }
    }

    proptest! {
    #[test]
        fn tokenize_literal(text in literal_strategy(), spaces in whitespace_strategy()) {
            let lexer = Lexer::new(&format!("{sp}{}{sp}", text.quote(), sp = spaces)).unwrap();
            let expected = Token::Literal(text).expected_sequence(&mut spaces.chars().count());
            prop_assert_eq!(&lexer[..], &expected[..]);
        }
    }

    proptest! {
        #[test]
        fn tokenize_identifier(name in identifier_strategy(), spaces in whitespace_strategy()) {
            let lexer = Lexer::new(&format!("{sp}{}{sp}", name, sp = spaces)).unwrap();
            let expected = Token::Identifier(name).expected_sequence(&mut spaces.chars().count());
            prop_assert_eq!(&lexer[..], &expected[..]);
        }
    }

    proptest! {
        #[test]
        fn tokenize_simple_expression(
            lhs: Numeric,
            rhs: Numeric,
            op: Operator,
            spaces in whitespace_strategy(),
        ) {
            let lexer = Lexer::new(&format!("{}{sp}{}{sp}{}", lhs, op, rhs, sp = spaces)).unwrap();
            let mut expected = vec![];

            let mut current_offset = 0;
            expected.extend(Token::Number(lhs).expected_sequence(&mut current_offset));
            current_offset += spaces.chars().count();
            expected.extend(Token::Operator(op).expected_sequence(&mut current_offset));
            current_offset += spaces.chars().count();
            expected.extend(Token::Number(rhs).expected_sequence(&mut current_offset));

            prop_assert_eq!(&lexer[..], &expected[..]);
        }
    }

    #[test]
    fn tokenize_bracket_pair() {
        let lexer = Lexer::new("()").unwrap();
        let expected = vec![
            Token::Bracket(Bracket::RoundOpen).at(0, 1),
            Token::Bracket(Bracket::RoundClose).at(1, 1),
        ];
        assert_eq!(&lexer[..], &expected[..]);
    }

    #[test]
    fn tokenize_complex_expression() {
        let lexer = Lexer::new("0+ fn3(1.5 + 10* -200, 5)/3^你好! \"Hi\"").unwrap();
        let expected = vec![
            Token::Number(0.0.into()).at(0, 1),
            Token::Operator(Operator::Plus).at(1, 1),
            Token::Identifier("fn3".to_owned()).at(3, 3),
            Token::Bracket(Bracket::RoundOpen).at(6, 1),
            Token::Number(1.5.into()).at(7, 3),
            Token::Operator(Operator::Plus).at(11, 1),
            Token::Number(10.0.into()).at(13, 2),
            Token::Operator(Operator::Star).at(15, 1),
            Token::Operator(Operator::Minus).at(17, 1),
            Token::Number(200.0.into()).at(18, 3),
            Token::Operator(Operator::Comma).at(21, 1),
            Token::Number(5.0.into()).at(23, 1),
            Token::Bracket(Bracket::RoundClose).at(24, 1),
            Token::Operator(Operator::Slash).at(25, 1),
            Token::Number(3.0.into()).at(26, 1),
            Token::Operator(Operator::Caret).at(27, 1),
            Token::Identifier("你好".to_owned()).at(28, 2),
            Token::Operator(Operator::ExclamationMark).at(30, 1),
            Token::Literal("Hi".to_owned()).at(32, 4),
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

            let mut current_offset = 0;
            let expected: Vec<_> = input
                .into_iter()
                .flat_map(|(token, space)| {
                    let tokens = token.expected_sequence(&mut current_offset);
                    current_offset += space.chars().count();
                    tokens
                })
                .collect();

            let lexer = Lexer::new(&token_str).unwrap();
            assert_eq!(&lexer[..], &expected[..]);
        }
    }
}
