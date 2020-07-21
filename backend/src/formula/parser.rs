use std::fmt;
use std::ops::Deref;

use super::error::FormulaError;
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
    Pow,
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
            Self::Pow => "^",
            Self::Pos => "⊕",
            Self::Neg => "⊖",
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

type PeekableToken<'a> = std::iter::Peekable<std::slice::Iter<'a, Token>>;

macro_rules! error {
    ($($arg:tt)*) => {{
        return Err(FormulaError::ParserError(format!($($arg)*)))
    }}
}

impl Parser {
    pub fn new(tokens: &[Token]) -> Result<Self, FormulaError> {
        let mut it = tokens.iter().peekable();
        let parsed_expression = Self::expression(&mut it, 0)?;

        if it.next().is_some() {
            error!("unparsed tokens at end of expression");
        }

        Ok(Self { parsed_expression })
    }

    fn expression(it: &mut PeekableToken, min_bp: u8) -> Result<Vec<ParseItem>, FormulaError> {
        let mut result = vec![];

        match it.next() {
            Some(Token::Number(value)) => result.push(ParseItem::Value(Value::Number(*value))),
            Some(Token::Identifier(name)) => {
                if let Some(Token::Bracket(LexerBracket::RoundOpen)) = it.peek() {
                    let op = Self::function_operator(name)?;
                    let bp = Self::function_binding_power();

                    result.extend(Self::expression(it, bp)?);
                    result.push(ParseItem::Operator(op));
                } else {
                    result.push(ParseItem::Value(Value::Variable(name.clone())))
                }
            }
            Some(Token::Operator(op)) => {
                let op = match op {
                    LexerOperator::Plus => Operator::Pos,
                    LexerOperator::Minus => Operator::Neg,
                    _ => error!("unsupported unary operator: {}", op),
                };
                let bp = Self::prefix_binding_power(&op);

                result.extend(Self::expression(it, bp)?);
                result.push(ParseItem::Operator(op));
            }
            Some(Token::Bracket(LexerBracket::RoundOpen)) => {
                result.extend(Self::expression(it, 0)?);
                match it.next() {
                    Some(Token::Bracket(LexerBracket::RoundClose)) => (),
                    Some(x) => panic!("expected closing bracket, found: {}", x),
                    None => error!("expected closing bracket, found end of expression"),
                }
            }
            Some(token) => error!("unsupported start token: {}", token),
            None => error!("unexpected end of expression"),
        };

        loop {
            match it.peek() {
                Some(Token::Bracket(LexerBracket::RoundClose)) => break,
                Some(Token::Operator(op)) => {
                    let op = match op {
                        LexerOperator::Plus => Operator::Add,
                        LexerOperator::Minus => Operator::Sub,
                        LexerOperator::Star => Operator::Mul,
                        LexerOperator::Slash => Operator::Div,
                        LexerOperator::Caret => Operator::Pow,
                    };
                    let (l_bp, r_bp) = Self::infix_binding_power(&op);
                    if l_bp < min_bp {
                        break;
                    }

                    it.next();
                    result.extend(Self::expression(it, r_bp)?);
                    result.push(ParseItem::Operator(op));
                }
                Some(token) => error!("unsupported start token: {}", token),
                None => break,
            };
        }

        Ok(result)
    }

    fn function_binding_power() -> u8 {
        7
    }

    fn prefix_binding_power(op: &Operator) -> u8 {
        match op {
            Operator::Pos | Operator::Neg => 7,
            _ => panic!("unsupported unary operator: {}", op),
        }
    }

    fn infix_binding_power(op: &Operator) -> (u8, u8) {
        match op {
            Operator::Add | Operator::Sub => (1, 2),
            Operator::Mul | Operator::Div => (3, 4),
            Operator::Pow => (5, 6),
            _ => panic!("unsupported binary operator: {}", op),
        }
    }

    fn function_operator(name: &String) -> Result<Operator, FormulaError> {
        match name.to_lowercase().as_str() {
            "sqrt" => Ok(Operator::Sqrt),
            _ => error!("unsupported function: {}", name),
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
pub mod tests {
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
    pub enum TokenTree {
        Number(f64),
        Variable(String),
        Expression(LexerOperator, Vec<TokenTree>),
        Function(String, Vec<TokenTree>),
    }

    impl TokenTree {
        fn postfix(&self) -> String {
            match self {
                TokenTree::Number(value) => value.to_string(),
                TokenTree::Variable(name) => name.to_string(),
                TokenTree::Expression(operator, operands) => operands
                    .iter()
                    .map(TokenTree::postfix)
                    .chain(once(match operator {
                        LexerOperator::Plus if operands.len() == 1 => "⊕".to_owned(),
                        LexerOperator::Minus if operands.len() == 1 => "⊖".to_owned(),
                        _ => operator.to_string(),
                    }))
                    .join(" "),
                TokenTree::Function(name, operands) => operands
                    .iter()
                    .map(TokenTree::postfix)
                    .chain(once(name.clone()))
                    .join(" "),
            }
        }

        pub fn infix(&self) -> Vec<Token> {
            match self {
                TokenTree::Number(value) => vec![Token::Number(*value)],
                TokenTree::Variable(name) => vec![Token::Identifier(name.clone())],
                TokenTree::Expression(operator, operands) => match operands.len() {
                    1 => vec![
                        vec![Token::Bracket(LexerBracket::RoundOpen)],
                        vec![Token::Operator(operator.clone())],
                        operands[0].infix(),
                        vec![Token::Bracket(LexerBracket::RoundClose)],
                    ],
                    2 => vec![
                        vec![Token::Bracket(LexerBracket::RoundOpen)],
                        operands[0].infix(),
                        vec![Token::Operator(operator.clone())],
                        operands[1].infix(),
                        vec![Token::Bracket(LexerBracket::RoundClose)],
                    ],
                    i => panic!("unsupported number of operands: {}", i),
                }
                .into_iter()
                .flatten()
                .collect(),
                TokenTree::Function(name, operands) => once(Token::Identifier(name.clone()))
                    .chain(once(Token::Bracket(LexerBracket::RoundOpen)))
                    .chain(operands.iter().flat_map(TokenTree::infix))
                    .chain(once(Token::Bracket(LexerBracket::RoundClose)))
                    .collect(),
            }
        }

        pub fn variables(self) -> Vec<String> {
            match self {
                TokenTree::Number(_) => vec![],
                TokenTree::Variable(name) => vec![name],
                TokenTree::Expression(_, operands) => operands
                    .into_iter()
                    .map(TokenTree::variables)
                    .flatten()
                    .collect(),
                TokenTree::Function(_, operands) => operands
                    .into_iter()
                    .map(TokenTree::variables)
                    .flatten()
                    .collect(),
            }
        }
    }

    fn unary_operator() -> BoxedStrategy<LexerOperator> {
        prop_oneof![Just(LexerOperator::Plus), Just(LexerOperator::Minus),].boxed()
    }

    fn unary_function() -> BoxedStrategy<String> {
        prop_oneof![Just("sqrt".to_owned()),].boxed()
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

    prop_compose! {
        fn unary_function_expression(base: BoxedStrategy<TokenTree>)
                          (name in unary_function(),
                                operands in prop::collection::vec(base, 1))
                          -> TokenTree {
           TokenTree::Function(name, operands)
       }
    }

    impl Arbitrary for TokenTree {
        type Parameters = ();
        type Strategy = BoxedStrategy<Self>;

        fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
            let leaf = prop_oneof![
                (0..100u32).prop_map(|v| TokenTree::Number(v as f64)),
                r"[[:lower:]]{1}".prop_map(TokenTree::Variable),
            ];

            leaf.prop_recursive(32, 1024, 2, |inner| {
                prop_oneof![
                    unary_expression(inner.clone()),
                    binary_expression(inner.clone()),
                    unary_function_expression(inner.clone()),
                ]
            })
            .boxed()
        }
    }

    #[test]
    fn parse_invalid() {
        assert!(Parser::new(&vec![Token::Operator(LexerOperator::Plus)]).is_err());
        assert!(Parser::new(&vec![
            Token::Number(1.0),
            Token::Bracket(LexerBracket::RoundClose)
        ])
        .is_err());
    }

    proptest! {
        #[test]
        fn parse_value(value: f64) {
            let tokens = vec![Token::Number(value)];
            let parser = Parser::new(&tokens).unwrap();
            prop_assert_eq!(parser.postfix(), value.to_string());
        }
    }

    proptest! {
        #[test]
        fn parse_variable(name: String) {
            let tokens = vec![Token::Identifier(name.clone())];
            let parser = Parser::new(&tokens).unwrap();
            prop_assert_eq!(parser.postfix(), name);
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
            Token::Operator(LexerOperator::Plus),
            Token::Number(2.0),
            Token::Operator(LexerOperator::Minus),
            Token::Number(3.0),
        ])
        .unwrap();
        assert_eq!(parser.postfix(), format!("1 2 + 3 -"));

        let parser = Parser::new(&vec![
            Token::Number(1.0),
            Token::Operator(LexerOperator::Plus),
            Token::Number(2.0),
            Token::Operator(LexerOperator::Star),
            Token::Number(3.0),
        ])
        .unwrap();
        assert_eq!(parser.postfix(), format!("1 2 3 * +"));

        let parser = Parser::new(&vec![
            Token::Number(1.0),
            Token::Operator(LexerOperator::Star),
            Token::Number(2.0),
            Token::Operator(LexerOperator::Minus),
            Token::Number(3.0),
        ])
        .unwrap();
        assert_eq!(parser.postfix(), format!("1 2 * 3 -"));

        let parser = Parser::new(&vec![
            Token::Bracket(LexerBracket::RoundOpen),
            Token::Number(1.0),
            Token::Operator(LexerOperator::Plus),
            Token::Number(2.0),
            Token::Bracket(LexerBracket::RoundClose),
            Token::Operator(LexerOperator::Star),
            Token::Number(3.0),
        ])
        .unwrap();
        assert_eq!(parser.postfix(), format!("1 2 + 3 *"));
    }

    #[test]
    fn parse_function() {
        let parser = Parser::new(&vec![
            Token::Identifier("sqrt".to_owned()),
            Token::Bracket(LexerBracket::RoundOpen),
            Token::Number(1.0),
            Token::Bracket(LexerBracket::RoundClose),
        ])
        .unwrap();
        assert_eq!(parser.postfix(), format!("1 sqrt"));
    }

    proptest! {
        #![proptest_config(ProptestConfig {
            max_shrink_iters: 2048,
            .. ProptestConfig::default()
        })]
        #[test]
        fn arbitrary_expression(token_tree: TokenTree) {
            let parser = Parser::new(&token_tree.infix()).unwrap();
            assert_eq!(parser.postfix(), token_tree.postfix());
        }
    }

    proptest! {
        #[test]
        fn tokenizer_does_not_crash(input in prop::collection::vec(any::<Token>(), 0..64)) {
            let _ = Parser::new(&input);
        }
    }
}
