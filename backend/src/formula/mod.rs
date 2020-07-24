pub mod error;
pub mod lexer;
pub mod parser;
mod quoted_string;

use std::collections::HashMap;

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

macro_rules! var {
    ($vars:ident, $name:ident) => {
        ($vars)
            .as_ref()
            .map(|m| m.get($name))
            .flatten()
            .ok_or(FormulaError::EvaluationError(format!(
                "variable {} not found",
                $name
            )))?
            .clone();
    };
}

type VariableDict = HashMap<String, f64>;

impl Formula {
    pub fn new(input: &str) -> Result<Self, FormulaError> {
        let lexer = lexer::Lexer::new(input)?;
        let parser = parser::Parser::new(&lexer[..])?;
        Ok(Self { parser })
    }

    pub fn eval(&self) -> Result<f64, FormulaError> {
        self.eval_internal(None)
    }

    pub fn eval_with(&self, vars: &VariableDict) -> Result<f64, FormulaError> {
        self.eval_internal(Some(vars))
    }

    fn eval_internal(&self, vars: Option<&VariableDict>) -> Result<f64, FormulaError> {
        let mut stack: Vec<f64> = vec![];
        for item in self.parser.iter() {
            match item {
                parser::ParseItem::Value(v) => match v {
                    parser::Value::Number(value) => stack.push(*value),
                    parser::Value::Literal(_) => stack.push(0.0),
                    parser::Value::Variable(name) => {
                        let var = var!(vars, name);
                        stack.push(var);
                    }
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
                    parser::Operator::Pow => {
                        let rhs = take!(stack);
                        let lhs = take!(stack);
                        stack.push(lhs.powf(rhs));
                    }
                    parser::Operator::Pos => {}
                    parser::Operator::Neg => {
                        let rhs = take!(stack);
                        stack.push(-rhs);
                    }
                    parser::Operator::Fac => {
                        // all factorials larger than `170!` will overflow an `f64`
                        let lhs = take!(stack);
                        stack.push(match lhs {
                            _ if lhs < 0.0 => std::f64::NAN,
                            _ if lhs > 170.0 => std::f64::INFINITY,
                            _ => (1..=(lhs as u32)).fold(1.0, |a, b| a * b as f64),
                        });
                    }
                },
                parser::ParseItem::Function(f, _) => match f {
                    parser::Function::Sqrt => {
                        let param = take!(stack);
                        stack.push(param.sqrt());
                    }
                    parser::Function::Round => {
                        let precision = take!(stack);
                        let value = take!(stack);
                        let factor = 10f64.powf(precision.trunc());
                        stack.push((value * factor).round() / factor);
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
    use proptest::prelude::*;

    #[test]
    fn evaluate_addition() {
        let formula = Formula::new("1 + 2").unwrap();
        assert_eq!(formula.eval().unwrap(), 3.0)
    }

    #[test]
    fn evaluate_subtraction() {
        let formula = Formula::new("1 - 2").unwrap();
        assert_eq!(formula.eval().unwrap(), -1.0)
    }

    #[test]
    fn evaluate_multiplication() {
        let formula = Formula::new("1 * 2").unwrap();
        assert_eq!(formula.eval().unwrap(), 2.0)
    }

    #[test]
    fn evaluate_division() {
        let formula = Formula::new("1 / 2").unwrap();
        assert_eq!(formula.eval().unwrap(), 0.5)
    }

    #[test]
    fn evaluate_power() {
        let formula = Formula::new("2 ^ 2").unwrap();
        assert_eq!(formula.eval().unwrap(), 4.0)
    }

    #[test]
    fn evaluate_non_negation() {
        let formula = Formula::new("+ 1").unwrap();
        assert_eq!(formula.eval().unwrap(), 1.0)
    }

    #[test]
    fn evaluate_negation() {
        let formula = Formula::new("- 1").unwrap();
        assert_eq!(formula.eval().unwrap(), -1.0)
    }

    #[test]
    fn evaluate_factorial() {
        let formula = Formula::new("3 !").unwrap();
        assert_eq!(formula.eval().unwrap(), 6.0)
    }

    #[test]
    fn evaluate_sqrt() {
        let formula = Formula::new("sqrt(4)").unwrap();
        assert_eq!(formula.eval().unwrap(), 2.0)
    }

    #[test]
    fn evaluate_round() {
        let formula = Formula::new("round(1.0001, 2)").unwrap();
        assert_eq!(formula.eval().unwrap(), 1.0);

        let formula = Formula::new("round(3.141592, 4)").unwrap();
        assert_eq!(formula.eval().unwrap(), 3.1416);
    }

    #[test]
    fn evaluate_complex_expression() {
        let formula = Formula::new("sqrt(3*3 + (6/3+2)*4) - 1").unwrap();
        assert_eq!(formula.eval().unwrap(), 4.0)
    }

    #[test]
    fn evaluate_var() {
        let vars: HashMap<String, f64> = [("a", 1.0), ("b", 2.0), ("c", 3.0)]
            .iter()
            .map(|(k, v)| (k.to_owned().to_owned(), *v))
            .collect();
        let formula = Formula::new("a + b * c").unwrap();
        let result = formula.eval_with(&vars).unwrap();
        assert_eq!(result, 7.0)
    }

    proptest! {
        #![proptest_config(ProptestConfig {
            max_shrink_iters: 2048,
            .. ProptestConfig::default()
        })]
        #[test]
        fn evaluate_arbitrary_expression(token_tree: parser::tests::TokenTree) {
            let infix_str = token_tree.infix_str();

            let mut vars = VariableDict::new();
            for var in token_tree.variables() {
                vars.insert(var, 0.0);
            }

            let formula = Formula::new(&infix_str).unwrap();
            formula.eval_with(&vars).unwrap();
        }
    }
}
