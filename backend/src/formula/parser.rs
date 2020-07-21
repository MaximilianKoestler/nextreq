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
    Fac,
    Sqrt,
    Round,
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
            Self::Fac => "!",
            Self::Sqrt => "sqrt",
            Self::Round => "round",
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
                Some(Token::Operator(LexerOperator::Comma)) => {
                    let (l_bp, r_bp) = Self::comma_binding_power();
                    if l_bp < min_bp {
                        break;
                    }

                    it.next();
                    result.extend(Self::expression(it, r_bp)?);
                }
                Some(Token::Operator(op)) => {
                    let op = match op {
                        LexerOperator::Plus => Operator::Add,
                        LexerOperator::Minus => Operator::Sub,
                        LexerOperator::Star => Operator::Mul,
                        LexerOperator::Slash => Operator::Div,
                        LexerOperator::Caret => Operator::Pow,
                        LexerOperator::ExclamationMark => Operator::Fac,
                        LexerOperator::Comma => panic!("unreachable code"),
                    };

                    if let Some(l_bp) = Self::postfix_binding_power(&op) {
                        if l_bp < min_bp {
                            break;
                        }
                        it.next();
                    } else {
                        let (l_bp, r_bp) = Self::infix_binding_power(&op);
                        if l_bp < min_bp {
                            break;
                        }

                        it.next();
                        result.extend(Self::expression(it, r_bp)?);
                    }
                    result.push(ParseItem::Operator(op));
                }
                Some(token) => error!("unsupported start token: {}", token),
                None => break,
            };
        }

        Ok(result)
    }

    fn function_binding_power() -> u8 {
        255
    }

    fn comma_binding_power() -> (u8, u8) {
        (1, 2)
    }

    fn prefix_binding_power(op: &Operator) -> u8 {
        match op {
            Operator::Pos | Operator::Neg => 9,
            _ => panic!("unsupported unary operator: {}", op),
        }
    }

    fn infix_binding_power(op: &Operator) -> (u8, u8) {
        match op {
            Operator::Add | Operator::Sub => (3, 4),
            Operator::Mul | Operator::Div => (5, 6),
            Operator::Pow => (7, 8),
            _ => panic!("unsupported binary operator: {}", op),
        }
    }

    fn postfix_binding_power(op: &Operator) -> Option<u8> {
        match op {
            Operator::Fac => Some(11),
            _ => None,
        }
    }

    fn function_operator(name: &String) -> Result<Operator, FormulaError> {
        match name.to_lowercase().as_str() {
            "sqrt" => Ok(Operator::Sqrt),
            "round" => Ok(Operator::Round),
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
        Expression(
            Option<Box<TokenTree>>,
            LexerOperator,
            Option<Box<TokenTree>>,
        ),
        Function(String, Vec<TokenTree>),
    }

    fn unary_operator_str(op: &LexerOperator) -> &'static str {
        match op {
            LexerOperator::Plus => "⊕",
            LexerOperator::Minus => "⊖",
            _ => panic!("not a unary operator: {}", op),
        }
    }

    impl TokenTree {
        fn postfix(&self) -> String {
            match self {
                TokenTree::Number(value) => value.to_string(),
                TokenTree::Variable(name) => name.to_string(),
                TokenTree::Expression(lhs, operator, rhs) => once(lhs)
                    .chain(once(rhs))
                    .filter_map(|t| t.as_ref())
                    .map(|t| t.postfix())
                    .chain(once(match operator {
                        op if lhs.is_none() => unary_operator_str(op).to_owned(),
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
                TokenTree::Expression(lhs, operator, rhs) => match (lhs.is_some(), rhs.is_some()) {
                    (false, true) => vec![
                        vec![Token::Bracket(LexerBracket::RoundOpen)],
                        vec![Token::Operator(operator.clone())],
                        rhs.as_ref().unwrap().infix(),
                        vec![Token::Bracket(LexerBracket::RoundClose)],
                    ],
                    (true, true) => vec![
                        vec![Token::Bracket(LexerBracket::RoundOpen)],
                        lhs.as_ref().unwrap().infix(),
                        vec![Token::Operator(operator.clone())],
                        rhs.as_ref().unwrap().infix(),
                        vec![Token::Bracket(LexerBracket::RoundClose)],
                    ],
                    (true, false) => vec![
                        vec![Token::Bracket(LexerBracket::RoundOpen)],
                        lhs.as_ref().unwrap().infix(),
                        vec![Token::Operator(operator.clone())],
                        vec![Token::Bracket(LexerBracket::RoundClose)],
                    ],
                    (false, false) => panic!("expression without operands!"),
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
                TokenTree::Expression(lhs, _, rhs) => once(lhs)
                    .chain(once(rhs))
                    .filter_map(|t| t)
                    .map(|t| t.variables())
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

    fn unary_prefix_operator() -> BoxedStrategy<LexerOperator> {
        prop_oneof![Just(LexerOperator::Plus), Just(LexerOperator::Minus),].boxed()
    }

    fn binary_infix_operator() -> BoxedStrategy<LexerOperator> {
        prop_oneof![
            Just(LexerOperator::Plus),
            Just(LexerOperator::Minus),
            Just(LexerOperator::Star),
            Just(LexerOperator::Slash),
            Just(LexerOperator::Caret),
        ]
        .boxed()
    }

    fn unary_postfix_operator() -> BoxedStrategy<LexerOperator> {
        prop_oneof![Just(LexerOperator::ExclamationMark),].boxed()
    }

    fn unary_function() -> BoxedStrategy<String> {
        prop_oneof![Just("sqrt".to_owned()),].boxed()
    }

    prop_compose! {
        fn unary_prefix_expression(base: BoxedStrategy<TokenTree>)
                          (operator in unary_prefix_operator(),
                                rhs in base)
                          -> TokenTree {

           TokenTree::Expression(None, operator, Some(Box::new(rhs)))
       }
    }

    prop_compose! {
        fn binary_infix_expression(base: BoxedStrategy<TokenTree>)
                          (operator in binary_infix_operator(),
                                lhs in base.clone(),
                                rhs in base)
                          -> TokenTree {
            TokenTree::Expression(Some(Box::new(lhs)), operator, Some(Box::new(rhs)))
       }
    }

    prop_compose! {
        fn unary_postfix_expression(base: BoxedStrategy<TokenTree>)
                          (operator in unary_postfix_operator(),
                                lhs in base)
                          -> TokenTree {
           TokenTree::Expression(Some(Box::new(lhs)), operator, None)
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
                    unary_prefix_expression(inner.clone()),
                    unary_postfix_expression(inner.clone()),
                    binary_infix_expression(inner.clone()),
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
        fn parse_binary_expression(lhs: f64, rhs: f64, op in binary_infix_operator()) {
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

    proptest! {
        #[test]
        fn parse_unary_prefix_expression(rhs: f64, op in unary_prefix_operator()) {
            let expected = format!("{} {}", rhs, unary_operator_str(&op));

            let tokens = vec![
                Token::Operator(op),
                Token::Number(rhs),
            ];

            let parser = Parser::new(&tokens).unwrap();
            assert_eq!(parser.postfix(), expected);
        }
    }

    proptest! {
        #[test]
        fn parse_unary_postfix_expression(lhs: f64, op in unary_postfix_operator()) {
            let expected = format!("{} {}", lhs, &op);

            let tokens = vec![
                Token::Number(lhs),
                Token::Operator(op),
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
    fn parse_single_parameter_function() {
        let parser = Parser::new(&vec![
            Token::Identifier("sqrt".to_owned()),
            Token::Bracket(LexerBracket::RoundOpen),
            Token::Number(1.0),
            Token::Bracket(LexerBracket::RoundClose),
        ])
        .unwrap();
        assert_eq!(parser.postfix(), format!("1 sqrt"));
    }

    #[test]
    fn parse_multi_parameter_function() {
        let parser = Parser::new(&vec![
            Token::Identifier("round".to_owned()),
            Token::Bracket(LexerBracket::RoundOpen),
            Token::Number(1.5),
            Token::Operator(LexerOperator::Comma),
            Token::Number(2.0),
            Token::Bracket(LexerBracket::RoundClose),
        ])
        .unwrap();
        assert_eq!(parser.postfix(), format!("1.5 2 round"));

        // TODO:
        // - test more parameters
        // - too few/many parameters causes error
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
