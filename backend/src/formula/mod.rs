pub mod error;
pub mod lexer;
pub mod parser;

use error::FormulaError;

pub struct Formula {
    parser: parser::Parser,
}

impl Formula {
    pub fn new(input: &str) -> Result<Self, FormulaError> {
        let lexer = lexer::Lexer::new(input)?;
        let parser = parser::Parser::new(&lexer[..])?;
        Ok(Self { parser })
    }

    pub fn eval(&self) -> f64 {
        let mut stack: Vec<f64> = vec![];
        for item in self.parser.iter() {
            match item {
                parser::ParseItem::Value(v) => match v {
                    parser::Value::Number(value) => stack.push(*value),
                    parser::Value::Variable(name) => stack.push(0.0),
                },
                parser::ParseItem::Operator(op) => match op {
                    parser::Operator::Add => {
                        let lhs = stack.pop().unwrap();
                        let rhs = stack.pop().unwrap();
                        stack.push(lhs + rhs);
                    }
                    parser::Operator::Sub => {
                        let lhs = stack.pop().unwrap();
                        let rhs = stack.pop().unwrap();
                        stack.push(0.0);
                    }
                    parser::Operator::Mul => {
                        let lhs = stack.pop().unwrap();
                        let rhs = stack.pop().unwrap();
                        stack.push(0.0);
                    }
                    parser::Operator::Div => {
                        let lhs = stack.pop().unwrap();
                        let rhs = stack.pop().unwrap();
                        stack.push(0.0);
                    }
                    parser::Operator::Pos => {}
                    parser::Operator::Neg => {}
                    parser::Operator::Sqrt => {}
                },
            }
        }
        assert_eq!(stack.len(), 1);
        return stack.pop().unwrap();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use itertools::Itertools;
    use proptest::prelude::*;

    #[test]
    fn evaluate_simple_addition() {
        let formula = Formula::new("1 + 1").unwrap();
        let result = formula.eval();
        assert_eq!(result, 2.0)
    }

    proptest! {
        #![proptest_config(ProptestConfig {
            max_shrink_iters: 2048,
            .. ProptestConfig::default()
        })]
        #[test]
        fn evaluate_arbitrary_expression(token_tree: parser::tests::TokenTree) {
            let infix_str = token_tree.infix().iter().map(lexer::Token::to_string).join("");
            let formula = Formula::new(&infix_str).unwrap();
            formula.eval();
        }
    }
}
