pub mod error;
pub mod lexer;
pub mod parser;

use error::FormulaError;

pub struct Formula {
    parser: parser::Parser,
}

macro_rules! take {
    ($stack:ident) => {
        ($stack).pop().ok_or(FormulaError::EvaluationError(
            "expected value, found empty stack".to_owned(),
        ))?;
    };
}

impl Formula {
    pub fn new(input: &str) -> Result<Self, FormulaError> {
        let lexer = lexer::Lexer::new(input)?;
        let parser = parser::Parser::new(&lexer[..])?;
        Ok(Self { parser })
    }

    pub fn eval(&self) -> Result<f64, FormulaError> {
        let mut stack: Vec<f64> = vec![];
        for item in self.parser.iter() {
            match item {
                parser::ParseItem::Value(v) => match v {
                    parser::Value::Number(value) => stack.push(*value),
                    parser::Value::Variable(name) => stack.push(0.0),
                },
                parser::ParseItem::Operator(op) => match op {
                    parser::Operator::Add => {
                        let rhs = take!(stack);
                        let lhs = take!(stack);
                        stack.push(lhs + rhs);
                    }
                    parser::Operator::Sub => {
                        let rhs = take!(stack);
                        let lhs = take!(stack);
                        stack.push(lhs - rhs);
                    }
                    parser::Operator::Mul => {
                        let rhs = take!(stack);
                        let lhs = take!(stack);
                        stack.push(lhs * rhs);
                    }
                    parser::Operator::Div => {
                        let rhs = take!(stack);
                        let lhs = take!(stack);
                        stack.push(lhs / rhs);
                    }
                    parser::Operator::Pos => {}
                    parser::Operator::Neg => {
                        let rhs = take!(stack);
                        stack.push(-rhs);
                    }
                    parser::Operator::Sqrt => {
                        let rhs = take!(stack);
                        stack.push(rhs.sqrt());
                    }
                },
            }
        }

        if stack.len() != 1 {
            Err(FormulaError::EvaluationError(format!(
                "expected exactly one item, found {}",
                stack.len()
            )))
        } else {
            Ok(stack.pop().unwrap())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use itertools::Itertools;
    use proptest::prelude::*;

    #[test]
    fn evaluate_addition() {
        let formula = Formula::new("1 + 2").unwrap();
        let result = formula.eval().unwrap();
        assert_eq!(result, 3.0)
    }

    #[test]
    fn evaluate_subtraction() {
        let formula = Formula::new("1 - 2").unwrap();
        let result = formula.eval().unwrap();
        assert_eq!(result, -1.0)
    }

    #[test]
    fn evaluate_multiplication() {
        let formula = Formula::new("1 * 2").unwrap();
        let result = formula.eval().unwrap();
        assert_eq!(result, 2.0)
    }

    #[test]
    fn evaluate_division() {
        let formula = Formula::new("1 / 2").unwrap();
        let result = formula.eval().unwrap();
        assert_eq!(result, 0.5)
    }

    #[test]
    fn evaluate_non_negation() {
        let formula = Formula::new("+ 1").unwrap();
        let result = formula.eval().unwrap();
        assert_eq!(result, 1.0)
    }

    #[test]
    fn evaluate_negation() {
        let formula = Formula::new("- 1").unwrap();
        let result = formula.eval().unwrap();
        assert_eq!(result, -1.0)
    }

    #[test]
    fn evaluate_sqrt() {
        let formula = Formula::new("sqrt (4)").unwrap();
        let result = formula.eval().unwrap();
        assert_eq!(result, 2.0)
    }

    #[test]
    fn evaluate_complex_expression() {
        let formula = Formula::new("sqrt(3*3 + (6/3+2)*4) - 1").unwrap();
        let result = formula.eval().unwrap();
        assert_eq!(result, 4.0)
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
            formula.eval().unwrap();
        }
    }
}
