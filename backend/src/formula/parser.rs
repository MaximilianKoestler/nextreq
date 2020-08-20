use std::fmt;
use std::ops::Deref;

use super::error::{ErrorMarker, FormulaError::ParserError, PositionedFormulaError};
use super::lexer::{Bracket as LexerBracket, Operator as LexerOperator, PositionedToken, Token};
use super::numeric::Numeric;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(Numeric),
    Literal(String),
    Variable(String),
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Number(n) => write!(f, "{}", n),
            Self::Literal(n) => write!(f, "\"{}\"", n),
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
        };
        write!(f, "{}", op)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseItem {
    Value(Value),
    Operator(Operator),
    Function(String, usize),
}

impl fmt::Display for ParseItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Value(x) => write!(f, "{}", x),
            Self::Operator(x) => write!(f, "{}", x),
            Self::Function(x, _) => write!(f, "{}", x),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct PositionedParseItem {
    pub item: ParseItem,
    pub start: usize,
    pub length: usize,
}

impl ErrorMarker for PositionedParseItem {
    fn start(&self) -> usize {
        self.start
    }
    fn end(&self) -> usize {
        self.start + self.length
    }
}

impl ParseItem {
    pub fn at(self, start: usize, length: usize) -> PositionedParseItem {
        PositionedParseItem {
            item: self,
            start,
            length,
        }
    }

    pub fn at_token(self, token: &PositionedToken) -> PositionedParseItem {
        PositionedParseItem {
            item: self,
            start: token.start,
            length: token.length,
        }
    }
}

impl fmt::Display for PositionedParseItem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.item)
    }
}

#[derive(Debug)]
pub struct Parser {
    parsed_expression: Vec<PositionedParseItem>,
}

type PeekableToken<'a> = std::iter::Peekable<std::slice::Iter<'a, PositionedToken>>;

struct TermInformation {
    _start: usize,
    term_count: usize,
}

impl TermInformation {
    fn new(start: usize) -> Self {
        Self {
            _start: start,
            term_count: 0,
        }
    }

    fn new_term(&mut self) {
        self.term_count += 1;
    }

    fn terms(&self) -> usize {
        self.term_count
    }
}

impl Parser {
    pub fn new(tokens: &[PositionedToken]) -> Result<Self, PositionedFormulaError> {
        let mut it = tokens.iter().peekable();
        let parsed_expression = Self::expression(&mut it, 0, &mut TermInformation::new(0))?;

        if let Some(token) = it.next() {
            return Err(
                ParserError(format!("unparsed tokens at end of expression")).at_marker(token)
            );
        }

        Ok(Self { parsed_expression })
    }

    fn expression(
        it: &mut PeekableToken,
        min_bp: u8,
        info: &mut TermInformation,
    ) -> Result<Vec<PositionedParseItem>, PositionedFormulaError> {
        let mut result = vec![];

        match it.next() {
            Some(token) => match &token.token {
                Token::Number(value) => {
                    result.push(ParseItem::Value(Value::Number(value.clone())).at_token(token))
                }
                Token::Literal(text) => {
                    result.push(ParseItem::Value(Value::Literal(text.clone())).at_token(token))
                }
                Token::Identifier(name) => {
                    if let Some(Token::Bracket(LexerBracket::RoundOpen)) =
                        it.peek().map(|t| &t.token)
                    {
                        let bp = Self::function_binding_power();

                        let mut new_info = TermInformation::new(token.start);

                        // handle special case of function without any parameter
                        let mut lookahead = it.clone();
                        lookahead.next();
                        if let Some(Token::Bracket(LexerBracket::RoundClose)) =
                            lookahead.next().map(|t| &t.token)
                        {
                            it.next();
                            it.next();
                        } else {
                            new_info.new_term();
                            result.extend(Self::expression(it, bp, &mut new_info)?);
                        }
                        result.push(
                            ParseItem::Function(name.to_ascii_lowercase(), new_info.terms())
                                .at_token(token),
                        );
                    } else {
                        result
                            .push(ParseItem::Value(Value::Variable(name.clone())).at_token(token));
                    }
                }
                Token::Operator(op) => {
                    let op = match op {
                        LexerOperator::Plus => Operator::Pos,
                        LexerOperator::Minus => Operator::Neg,
                        _ => {
                            return Err(ParserError(format!("unsupported unary operator: {}", op))
                                .at_marker(token))
                        }
                    };
                    let bp = Self::prefix_binding_power(&op);

                    result.extend(Self::expression(
                        it,
                        bp,
                        &mut TermInformation::new(token.start),
                    )?);
                    result.push(ParseItem::Operator(op).at_token(token));
                }
                Token::Bracket(LexerBracket::RoundOpen) => {
                    result.extend(Self::expression(it, 0, info)?);
                    match it.next().map(|t| &t.token) {
                        Some(Token::Bracket(LexerBracket::RoundClose)) => (),
                        Some(x) => panic!("expected closing bracket, found: {}", x),
                        None => {
                            return Err(ParserError(format!(
                                "expected closing bracket, found end of expression"
                            ))
                            .to_end(token.start))
                        }
                    }
                }
                _ => {
                    return Err(
                        ParserError(format!("unsupported start token: {}", token.token))
                            .at_marker(token),
                    )
                }
            },
            None => return Err(ParserError(format!("unexpected end of expression")).at_end()),
        };

        loop {
            match it.peek() {
                Some(&token) => match &token.token {
                    Token::Bracket(LexerBracket::RoundClose) => break,
                    Token::Operator(LexerOperator::Comma) => {
                        info.new_term();
                        let (l_bp, r_bp) = Self::comma_binding_power();
                        if l_bp < min_bp {
                            break;
                        }

                        it.next();
                        result.extend(Self::expression(
                            it,
                            r_bp,
                            &mut TermInformation::new(token.start),
                        )?);
                    }
                    Token::Operator(op) => {
                        let op = match op {
                            LexerOperator::Plus => Operator::Add,
                            LexerOperator::Minus => Operator::Sub,
                            LexerOperator::Star => Operator::Mul,
                            LexerOperator::Slash => Operator::Div,
                            LexerOperator::Caret => Operator::Pow,
                            LexerOperator::ExclamationMark => Operator::Fac,
                            LexerOperator::Comma => panic!("unreachable code"),
                        };

                        let (start, length) = (token.start, token.length);
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
                            result.extend(Self::expression(
                                it,
                                r_bp,
                                &mut TermInformation::new(token.start),
                            )?);
                        }
                        result.push(ParseItem::Operator(op).at(start, length));
                    }
                    _ => {
                        return Err(ParserError(format!(
                            "unsupported continuation token: {}",
                            token.token
                        ))
                        .at_marker(token))
                    }
                },
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
}

impl Deref for Parser {
    type Target = [PositionedParseItem];

    fn deref(&self) -> &Self::Target {
        &self.parsed_expression
    }
}

#[cfg(test)]
pub(crate) mod tests {
    use super::super::error::FormulaError::ParserError;
    use super::super::quoted_string::Quotable;
    use super::*;
    use itertools::Itertools;
    use proptest::prelude::*;
    use std::iter::{once, repeat};

    use crate::formula::lexer::tests::Positionable;

    impl Parser {
        fn postfix(&self) -> String {
            self.iter().map(|p| p.to_string()).join(" ")
        }

        fn positions(&self) -> Vec<(usize, usize)> {
            self.iter().map(|p| (p.start, p.length)).collect()
        }
    }

    pub enum TokenTree {
        Number(Numeric),
        Literal(String),
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
                Self::Number(value) => value.to_string(),
                Self::Literal(text) => text.quote(),
                Self::Variable(name) => name.to_string(),
                Self::Expression(lhs, operator, rhs) => once(lhs)
                    .chain(once(rhs))
                    .filter_map(|t| t.as_ref())
                    .map(|t| t.postfix())
                    .chain(once(match operator {
                        op if lhs.is_none() => unary_operator_str(op).to_owned(),
                        _ => operator.to_string(),
                    }))
                    .join(" "),
                Self::Function(name, operands) => operands
                    .iter()
                    .map(Self::postfix)
                    .chain(once(name.clone()))
                    .join(" "),
            }
        }

        pub fn infix(&self) -> Vec<Token> {
            match self {
                Self::Number(value) => vec![Token::Number(value.clone())],
                Self::Literal(value) => vec![Token::Literal(value.clone())],
                Self::Variable(name) => vec![Token::Identifier(name.clone())],
                Self::Expression(lhs, operator, rhs) => match (lhs.is_some(), rhs.is_some()) {
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
                Self::Function(name, operands) => {
                    let commas: Vec<_> = repeat(vec![Token::Operator(LexerOperator::Comma)])
                        .take(operands.len() - 1)
                        .collect();
                    let expanded_operands: Vec<_> = operands.iter().map(Self::infix).collect();
                    let separated_operands: Vec<_> = expanded_operands
                        .into_iter()
                        .interleave(commas.into_iter())
                        .collect();

                    once(Token::Identifier(name.clone()))
                        .chain(once(Token::Bracket(LexerBracket::RoundOpen)))
                        .chain(separated_operands.into_iter().flatten())
                        .chain(once(Token::Bracket(LexerBracket::RoundClose)))
                        .collect()
                }
            }
        }

        pub fn infix_str(&self) -> String {
            self.infix().iter().map(Token::to_string).join("")
        }

        pub fn variables(self) -> Vec<String> {
            match self {
                Self::Number(_) | Self::Literal(_) => vec![],
                Self::Variable(name) => vec![name],
                Self::Expression(lhs, _, rhs) => once(lhs)
                    .chain(once(rhs))
                    .filter_map(|t| t)
                    .map(|t| t.variables())
                    .flatten()
                    .collect(),
                Self::Function(_, operands) => operands
                    .into_iter()
                    .map(Self::variables)
                    .flatten()
                    .collect(),
            }
        }

        fn fmt_classic(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            match self {
                TokenTree::Number(x) => write!(f, "Number({:?}", x),
                TokenTree::Literal(x) => write!(f, "Literal({:?}", x),
                TokenTree::Variable(x) => write!(f, "Variable({:?}", x),
                TokenTree::Expression(lhs, op, rhs) => {
                    write!(f, "Expression(")?;
                    match lhs {
                        Some(b) => {
                            b.fmt_classic(f)?;
                        }
                        None => {
                            write!(f, "None")?;
                        }
                    }
                    write!(f, ", {:?}, ", op)?;
                    match rhs {
                        Some(b) => b.fmt_classic(f),
                        None => write!(f, "None"),
                    }
                }
                TokenTree::Function(name, operands) => {
                    write!(f, "Function({:?}, [", name)?;
                    let mut it = operands.iter().peekable();
                    while let Some(operand) = it.next() {
                        if it.peek().is_some() {
                            operand.fmt_classic(f)?;
                            write!(f, ", ")?;
                        } else {
                            operand.fmt_classic(f)?;
                        }
                    }
                    write!(f, "]")
                }
            }?;
            write!(f, ")")
        }
    }

    impl fmt::Debug for TokenTree {
        fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
            write!(f, "{} <=> ", self.infix_str())?;
            self.fmt_classic(f)
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

    fn function_name() -> BoxedStrategy<String> {
        r"[[:lower:]]{3}".boxed()
    }

    prop_compose! {
        fn function()
                   (name in function_name(),
                        parameters in (1..10usize))
                   -> (String, usize) {
            (name.to_owned(), parameters)
        }
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
        fn function_expression(base: BoxedStrategy<TokenTree>)
                (base in Just(base), (name, params) in function())
                (name in Just(name), operands in prop::collection::vec(base, params))
                -> TokenTree {
           TokenTree::Function(name, operands)
       }
    }

    impl Arbitrary for TokenTree {
        type Parameters = ();
        type Strategy = BoxedStrategy<Self>;

        fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
            let leaf = prop_oneof![
                (0..100u32).prop_map(|v| TokenTree::Number(v.into())),
                r"[[:lower:]]{3}".prop_map(TokenTree::Literal),
                r"[[:lower:]]{1}".prop_map(TokenTree::Variable),
            ];

            leaf.prop_recursive(8, 512, 2, |inner| {
                prop_oneof![
                    unary_prefix_expression(inner.clone()),
                    unary_postfix_expression(inner.clone()),
                    binary_infix_expression(inner.clone()),
                    function_expression(inner.clone()),
                ]
            })
            .boxed()
        }
    }

    #[test]
    fn parse_errors() {
        use super::super::error::ErrorPosition::*;

        // operator followed by nothing
        let error =
            Parser::new(&vec![Token::Operator(LexerOperator::Plus)].at_their_index()).unwrap_err();
        assert!(matches!(error.error, ParserError(_)));
        assert_eq!(error.start, End);
        assert_eq!(error.end, End);

        // random closing bracket at the end
        let error = Parser::new(
            &vec![
                Token::Number(1.0.into()),
                Token::Bracket(LexerBracket::RoundClose),
            ]
            .at_their_index(),
        )
        .unwrap_err();
        assert!(matches!(error.error, ParserError(_)));
        assert_eq!(error.start, Offset(1));
        assert_eq!(error.end, Offset(2));

        // * is not an unary operator
        let error = Parser::new(
            &vec![
                Token::Number(1.0.into()),
                Token::Operator(LexerOperator::Minus),
                Token::Operator(LexerOperator::Star),
                Token::Number(1.0.into()),
            ]
            .at_their_index(),
        )
        .unwrap_err();
        assert!(matches!(error.error, ParserError(_)));
        assert_eq!(error.start, Offset(2));
        assert_eq!(error.end, Offset(3));

        // missing closing bracket
        let error = Parser::new(
            &vec![
                Token::Number(1.0.into()),
                Token::Operator(LexerOperator::Plus),
                Token::Bracket(LexerBracket::RoundOpen),
                Token::Number(1.0.into()),
            ]
            .at_their_index(),
        )
        .unwrap_err();
        assert!(matches!(error.error, ParserError(_)));
        assert_eq!(error.start, Offset(2));
        assert_eq!(error.end, End);

        // closing bracket is not a valid start of an expression
        let error = Parser::new(
            &vec![
                Token::Number(1.0.into()),
                Token::Operator(LexerOperator::Plus),
                Token::Bracket(LexerBracket::RoundClose),
            ]
            .at_their_index(),
        )
        .unwrap_err();
        assert!(matches!(error.error, ParserError(_)));
        assert_eq!(error.start, Offset(2));
        assert_eq!(error.end, Offset(3));

        // expression ends unexpectedly
        let error = Parser::new(
            &vec![
                Token::Number(1.0.into()),
                Token::Operator(LexerOperator::Plus),
            ]
            .at_their_index(),
        )
        .unwrap_err();
        assert!(matches!(error.error, ParserError(_)));
        assert_eq!(error.start, End);
        assert_eq!(error.end, End);

        // missing operator between values
        let error = Parser::new(
            &vec![Token::Number(1.0.into()), Token::Number(1.0.into())].at_their_index(),
        )
        .unwrap_err();
        assert!(matches!(error.error, ParserError(_)));
        assert_eq!(error.start, Offset(1));
        assert_eq!(error.end, Offset(2));
    }

    #[test]
    fn item_positions() {
        let items = Parser::new(
            &vec![
                Token::Number(1.0.into()),
                Token::Operator(LexerOperator::Plus),
                Token::Number(1.0.into()),
            ]
            .at_their_index(),
        )
        .unwrap();
        assert_eq!(items.positions(), vec![(0, 1), (2, 1), (1, 1)]);

        let items = Parser::new(
            &vec![
                Token::Literal("a".to_owned()),
                Token::Operator(LexerOperator::Plus),
                Token::Literal("a".to_owned()),
            ]
            .at_their_index(),
        )
        .unwrap();
        assert_eq!(items.positions(), vec![(0, 1), (2, 1), (1, 1)]);

        let items = Parser::new(
            &vec![
                Token::Identifier("a".to_owned()),
                Token::Operator(LexerOperator::Plus),
                Token::Identifier("a".to_owned()),
            ]
            .at_their_index(),
        )
        .unwrap();
        assert_eq!(items.positions(), vec![(0, 1), (2, 1), (1, 1)]);

        let items = Parser::new(
            &vec![
                Token::Identifier("sqrt".to_owned()),
                Token::Bracket(LexerBracket::RoundOpen),
                Token::Number(1.0.into()),
                Token::Bracket(LexerBracket::RoundClose),
            ]
            .at_their_index(),
        )
        .unwrap();
        assert_eq!(items.positions(), vec![(2, 1), (0, 1)]);

        let items = Parser::new(
            &vec![
                Token::Operator(LexerOperator::Plus),
                Token::Number(1.0.into()),
            ]
            .at_their_index(),
        )
        .unwrap();
        assert_eq!(items.positions(), vec![(1, 1), (0, 1)]);
    }

    proptest! {
        #[test]
        fn parse_value(value: Numeric) {
            let tokens = vec![Token::Number(value.clone())].anywhere();
            let parser = Parser::new(&tokens).unwrap();
            prop_assert_eq!(parser.postfix(), value.to_string());
        }
    }

    proptest! {
        #[test]
        fn parse_literal(text: String) {
            let tokens = vec![Token::Literal(text.clone())].anywhere();
            let parser = Parser::new(&tokens).unwrap();
            let expected = format!("\"{}\"", text);
            prop_assert_eq!(parser.postfix(), expected);
        }
    }

    proptest! {
        #[test]
        fn parse_variable(name: String) {
            let tokens = vec![Token::Identifier(name.clone())].anywhere();
            let parser = Parser::new(&tokens).unwrap();
            prop_assert_eq!(parser.postfix(), name);
        }
    }

    #[test]
    fn parse_bracket_value() {
        let tokens = vec![
            Token::Bracket(LexerBracket::RoundOpen),
            Token::Number(1.0.into()),
            Token::Bracket(LexerBracket::RoundClose),
        ]
        .anywhere();
        let parser = Parser::new(&tokens).unwrap();
        assert_eq!(parser.postfix(), "1".to_owned());
    }

    proptest! {
        #[test]
        fn parse_binary_expression(lhs: Numeric, rhs: Numeric, op in binary_infix_operator()) {
            let expected = format!("{} {} {}", lhs, rhs, op);

            let tokens = vec![
                Token::Number(lhs),
                Token::Operator(op),
                Token::Number(rhs),
            ].anywhere();

            let parser = Parser::new(&tokens).unwrap();
            assert_eq!(parser.postfix(), expected);
        }
    }

    proptest! {
        #[test]
        fn parse_unary_prefix_expression(rhs: Numeric, op in unary_prefix_operator()) {
            let expected = format!("{} {}", rhs, unary_operator_str(&op));

            let tokens = vec![
                Token::Operator(op),
                Token::Number(rhs),
            ].anywhere();

            let parser = Parser::new(&tokens).unwrap();
            assert_eq!(parser.postfix(), expected);
        }
    }

    proptest! {
        #[test]
        fn parse_unary_postfix_expression(lhs: Numeric, op in unary_postfix_operator()) {
            let expected = format!("{} {}", lhs, &op);

            let tokens = vec![
                Token::Number(lhs),
                Token::Operator(op),
            ].anywhere();

            let parser = Parser::new(&tokens).unwrap();
            assert_eq!(parser.postfix(), expected);
        }
    }

    #[test]
    fn parse_complex_expressions() {
        let parser = Parser::new(
            &vec![
                Token::Number(1.0.into()),
                Token::Operator(LexerOperator::Plus),
                Token::Number(2.0.into()),
                Token::Operator(LexerOperator::Minus),
                Token::Number(3.0.into()),
            ]
            .anywhere(),
        )
        .unwrap();
        assert_eq!(parser.postfix(), format!("1 2 + 3 -"));

        let parser = Parser::new(
            &vec![
                Token::Number(1.0.into()),
                Token::Operator(LexerOperator::Plus),
                Token::Number(2.0.into()),
                Token::Operator(LexerOperator::Star),
                Token::Number(3.0.into()),
            ]
            .anywhere(),
        )
        .unwrap();
        assert_eq!(parser.postfix(), format!("1 2 3 * +"));

        let parser = Parser::new(
            &vec![
                Token::Number(1.0.into()),
                Token::Operator(LexerOperator::Star),
                Token::Number(2.0.into()),
                Token::Operator(LexerOperator::Minus),
                Token::Number(3.0.into()),
            ]
            .anywhere(),
        )
        .unwrap();
        assert_eq!(parser.postfix(), format!("1 2 * 3 -"));

        let parser = Parser::new(
            &vec![
                Token::Bracket(LexerBracket::RoundOpen),
                Token::Number(1.0.into()),
                Token::Operator(LexerOperator::Plus),
                Token::Number(2.0.into()),
                Token::Bracket(LexerBracket::RoundClose),
                Token::Operator(LexerOperator::Star),
                Token::Number(3.0.into()),
            ]
            .anywhere(),
        )
        .unwrap();
        assert_eq!(parser.postfix(), format!("1 2 + 3 *"));
    }

    #[test]
    fn parse_function_without_parameters() {
        let parser = Parser::new(
            &vec![
                Token::Identifier("now".to_owned()),
                Token::Bracket(LexerBracket::RoundOpen),
                Token::Bracket(LexerBracket::RoundClose),
            ]
            .at_their_index(),
        )
        .unwrap();
        let expected = vec![ParseItem::Function("now".to_owned(), 0).at(0, 1)];
        assert_eq!(&parser[..], &expected[..]);
    }

    #[test]
    fn parse_single_parameter_function() {
        let parser = Parser::new(
            &vec![
                Token::Identifier("sqrt".to_owned()),
                Token::Bracket(LexerBracket::RoundOpen),
                Token::Number(1.0.into()),
                Token::Bracket(LexerBracket::RoundClose),
            ]
            .at_their_index(),
        )
        .unwrap();
        let expected = vec![
            ParseItem::Value(Value::Number(1.0.into())).at(2, 1),
            ParseItem::Function("sqrt".to_owned(), 1).at(0, 1),
        ];
        assert_eq!(&parser[..], &expected[..]);

        let parser = Parser::new(
            &vec![
                Token::Identifier("abs".to_owned()),
                Token::Bracket(LexerBracket::RoundOpen),
                Token::Number(1.0.into()),
                Token::Bracket(LexerBracket::RoundClose),
            ]
            .at_their_index(),
        )
        .unwrap();
        let expected = vec![
            ParseItem::Value(Value::Number(1.0.into())).at(2, 1),
            ParseItem::Function("abs".to_owned(), 1).at(0, 1),
        ];
        assert_eq!(&parser[..], &expected[..]);

        let parser = Parser::new(
            &vec![
                Token::Identifier("SQRT".to_owned()),
                Token::Bracket(LexerBracket::RoundOpen),
                Token::Number(1.0.into()),
                Token::Operator(LexerOperator::Plus),
                Token::Number(2.0.into()),
                Token::Bracket(LexerBracket::RoundClose),
            ]
            .at_their_index(),
        )
        .unwrap();
        let expected = vec![
            ParseItem::Value(Value::Number(1.0.into())).at(2, 1),
            ParseItem::Value(Value::Number(2.0.into())).at(4, 1),
            ParseItem::Operator(Operator::Add).at(3, 1),
            ParseItem::Function("sqrt".to_owned(), 1).at(0, 1),
        ];
        assert_eq!(&parser[..], &expected[..]);
    }

    #[test]
    fn parse_multi_parameter_function() {
        let parser = Parser::new(
            &vec![
                Token::Identifier("round".to_owned()),
                Token::Bracket(LexerBracket::RoundOpen),
                Token::Number(1.5.into()),
                Token::Operator(LexerOperator::Comma),
                Token::Number(2.0.into()),
                Token::Bracket(LexerBracket::RoundClose),
            ]
            .at_their_index(),
        )
        .unwrap();
        let expected = vec![
            ParseItem::Value(Value::Number(1.5.into())).at(2, 1),
            ParseItem::Value(Value::Number(2.0.into())).at(4, 1),
            ParseItem::Function("round".to_owned(), 2).at(0, 1),
        ];
        assert_eq!(&parser[..], &expected[..]);

        let parser = Parser::new(
            &vec![
                Token::Identifier("RoUnD".to_owned()),
                Token::Bracket(LexerBracket::RoundOpen),
                Token::Bracket(LexerBracket::RoundOpen),
                Token::Number(1.5.into()),
                Token::Bracket(LexerBracket::RoundClose),
                Token::Operator(LexerOperator::Comma),
                Token::Number(2.0.into()),
                Token::Bracket(LexerBracket::RoundClose),
            ]
            .at_their_index(),
        )
        .unwrap();
        let expected = vec![
            ParseItem::Value(Value::Number(1.5.into())).at(3, 1),
            ParseItem::Value(Value::Number(2.0.into())).at(6, 1),
            ParseItem::Function("round".to_owned(), 2).at(0, 1),
        ];
        assert_eq!(&parser[..], &expected[..]);
    }

    #[test]
    fn parse_literal_operations() {
        let parser = Parser::new(
            &vec![
                Token::Literal("abc".to_owned()),
                Token::Operator(LexerOperator::Plus),
                Token::Literal("def".to_owned()),
                Token::Operator(LexerOperator::Minus),
                Token::Number(3.0.into()),
            ]
            .anywhere(),
        )
        .unwrap();
        assert_eq!(parser.postfix(), format!(r#""abc" "def" + 3 -"#));

        let parser = Parser::new(
            &vec![
                Token::Identifier("sqrt".to_owned()),
                Token::Bracket(LexerBracket::RoundOpen),
                Token::Literal("abc".to_owned()),
                Token::Bracket(LexerBracket::RoundClose),
            ]
            .anywhere(),
        )
        .unwrap();
        assert_eq!(parser.postfix(), format!(r#""abc" sqrt"#));
    }

    proptest! {
        #![proptest_config(ProptestConfig {
            max_shrink_iters: 2048,
            .. ProptestConfig::default()
        })]
        #[test]
        fn arbitrary_expression(token_tree: TokenTree) {
            let parser = Parser::new(&token_tree.infix().anywhere());
            prop_assert_eq!(parser.unwrap().postfix(), token_tree.postfix());
        }
    }

    proptest! {
        #[test]
        fn tokenizer_does_not_crash(input in prop::collection::vec(any::<Token>(), 0..64)) {
            let _ = Parser::new(&input.anywhere());
        }
    }
}
