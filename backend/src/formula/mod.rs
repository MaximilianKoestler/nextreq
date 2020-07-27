pub mod error;
pub mod lexer;
pub mod numeric;
pub mod parser;
mod quoted_string;

use std::cmp::Ordering;
use std::collections::HashMap;
use std::fmt;
use std::iter::repeat;
use std::ops::{Add, Mul};

use error::FormulaError;
use numeric::Numeric;
use quoted_string::Quotable;

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

macro_rules! error {
    ($($arg:tt)*) => {{
        return Err(FormulaError::EvaluationError(format!($($arg)*)))
    }}
}

macro_rules! enforce_number {
    ($description:literal, $var:ident) => {
        match $var {
            Number(value) => value,
            Literal(_) => error!("the {} is not supported for string literals", $description),
        }
    };

    ($description:literal, $var1:ident, $var2:ident) => {
        (
            match $var1 {
                Number(value) => value,
                Literal(_) => error!("the {} is not supported for string literals", $description),
            },
            match $var2 {
                Number(value) => value,
                Literal(_) => error!("the {} is not supported for string literals", $description),
            },
        )
    };
}

pub type VariableDict = HashMap<String, Numeric>;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(Numeric),
    Literal(String),
}

use Value::{Literal, Number};

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::Number(x) => write!(f, "{}", x),
            Self::Literal(x) => write!(f, "{}", x.quote()),
        }
    }
}

impl PartialOrd<Value> for Value {
    fn partial_cmp(&self, other: &Value) -> Option<Ordering> {
        match (self, other) {
            (Number(s), Number(o)) => s.partial_cmp(o),
            (Number(_), Literal(_)) => None,
            (Literal(_), Number(_)) => None,
            (Literal(s), Literal(o)) => s.partial_cmp(o),
        }
    }
}

impl Add for Value {
    type Output = Result<Self, FormulaError>;

    fn add(self, other: Self) -> Self::Output {
        match (self, other) {
            (Number(s), Number(o)) => Ok(Number(s + o)),
            (Number(_), Literal(_)) => error!("literals cannot be added to numbers"),
            (Literal(_), Number(_)) => error!("numbers cannot be added to literals"),
            (Literal(s), Literal(o)) => Ok(Literal(format!("{}{}", s, o))),
        }
    }
}

impl Mul for Value {
    type Output = Result<Self, FormulaError>;

    fn mul(self, other: Self) -> Self::Output {
        match (self, other) {
            (Number(s), Number(o)) => Ok(Number(s * o)),
            (Number(n), Literal(l)) | (Literal(l), Number(n)) => {
                if l.is_empty() {
                    return Ok(Literal(l));
                }

                let n = n.into();
                let single_length = l.len();
                const MAXIMIUM_LENGTH: usize = 4096;

                if let Some(combined_length) = single_length.checked_mul(n) {
                    if combined_length > MAXIMIUM_LENGTH {
                        error!(
                        "resulting string would be of length {} which is more than the maximum {}",
                        combined_length, MAXIMIUM_LENGTH
                    );
                    }
                    Ok(Literal(repeat(l).take(n).collect::<String>()))
                } else {
                    error!("computing the space requirements for {} literals of length {} results in an overflow", n, single_length)
                }
            }
            (Literal(_), Literal(_)) => error!("literals cannot be multiplied with literals"),
        }
    }
}

impl Formula {
    pub fn new(input: &str) -> Result<Self, FormulaError> {
        let lexer = lexer::Lexer::new(input)?;
        let parser = parser::Parser::new(&lexer[..])?;
        Ok(Self { parser })
    }

    pub fn eval(&self) -> Result<Value, FormulaError> {
        self.eval_internal(None)
    }

    pub fn eval_with(&self, vars: &VariableDict) -> Result<Value, FormulaError> {
        self.eval_internal(Some(vars))
    }

    fn eval_internal(&self, vars: Option<&VariableDict>) -> Result<Value, FormulaError> {
        let mut stack: Vec<Value> = vec![];
        for item in self.parser.iter() {
            match item {
                parser::ParseItem::Value(v) => match v {
                    parser::Value::Number(value) => stack.push(Number(value.clone())),
                    parser::Value::Literal(text) => stack.push(Literal(text.clone())),
                    parser::Value::Variable(name) => {
                        let var = var!(vars, name);
                        stack.push(Number(var));
                    }
                },
                parser::ParseItem::Operator(op) => match op {
                    parser::Operator::Add => {
                        let rhs = take!(stack);
                        let lhs = take!(stack);
                        stack.push((lhs + rhs)?);
                    }
                    parser::Operator::Sub => {
                        let rhs = take!(stack);
                        let lhs = take!(stack);
                        let (rhs, lhs) = enforce_number!("subtraction operator", rhs, lhs);
                        stack.push(Number(lhs - rhs));
                    }
                    parser::Operator::Mul => {
                        let rhs = take!(stack);
                        let lhs = take!(stack);
                        stack.push((lhs * rhs)?);
                    }
                    parser::Operator::Div => {
                        let rhs = take!(stack);
                        let lhs = take!(stack);
                        let (rhs, lhs) = enforce_number!("division operator", rhs, lhs);
                        stack.push(Number(lhs / rhs));
                    }
                    parser::Operator::Pow => {
                        let rhs = take!(stack);
                        let lhs = take!(stack);
                        let (rhs, lhs) = enforce_number!("power operator", rhs, lhs);

                        stack.push(Number(lhs.pow(&rhs)));
                    }
                    parser::Operator::Pos => {}
                    parser::Operator::Neg => {
                        let rhs = take!(stack);
                        let rhs = enforce_number!("negation operator", rhs);
                        stack.push(Number(-rhs));
                    }
                    parser::Operator::Fac => {
                        let lhs = take!(stack);
                        let lhs = enforce_number!("factorial operator", lhs);
                        stack.push(Number(lhs.factorial()));
                    }
                },
                parser::ParseItem::Function(f, _) => match f {
                    parser::Function::Sqrt => {
                        let param = take!(stack);
                        let param = enforce_number!("sqrt function", param);
                        stack.push(Number(param.sqrt()));
                    }
                    parser::Function::Abs => {
                        let param = take!(stack);
                        let param = enforce_number!("abs function", param);
                        stack.push(Number(param.abs()));
                    }
                    parser::Function::Round => {
                        let precision = take!(stack);
                        let value = take!(stack);
                        let (precision, value) =
                            enforce_number!("round function", precision, value);
                        let factor = Numeric::from(10.0).pow(&precision.trunc());
                        stack.push(Number((value * factor).round() / factor));
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
    fn value_ordering() {
        assert!(Number(3.0.into()) > Number(2.0.into()));
        assert_eq!(
            Number(3.0.into()).partial_cmp(&Literal("a".to_owned())),
            None
        );
        assert_eq!(
            Literal("a".to_owned()).partial_cmp(&Number(3.0.into())),
            None
        );
        assert!(Literal("b".to_owned()) > Literal("a".to_owned()));
    }

    #[test]
    fn evaluate_addition() {
        let formula = Formula::new("1 + 2").unwrap();
        assert_eq!(formula.eval().unwrap(), Number(3.0.into()));
    }

    #[test]
    fn evaluate_subtraction() {
        let formula = Formula::new("1 - 2").unwrap();
        assert_eq!(formula.eval().unwrap(), Number(Numeric::from(-1.0)));
    }

    #[test]
    fn evaluate_multiplication() {
        let formula = Formula::new("1 * 2").unwrap();
        assert_eq!(formula.eval().unwrap(), Number(2.0.into()));
    }

    #[test]
    fn evaluate_division() {
        let formula = Formula::new("1 / 2").unwrap();
        assert_eq!(formula.eval().unwrap(), Number(0.5.into()));
    }

    #[test]
    fn evaluate_power() {
        let formula = Formula::new("2 ^ 2").unwrap();
        assert_eq!(formula.eval().unwrap(), Number(4.0.into()));
    }

    #[test]
    fn evaluate_non_negation() {
        let formula = Formula::new("+ 1").unwrap();
        assert_eq!(formula.eval().unwrap(), Number(1.0.into()));
    }

    #[test]
    fn evaluate_negation() {
        let formula = Formula::new("- 1").unwrap();
        assert_eq!(formula.eval().unwrap(), Number(Numeric::from(-1.0)));
    }

    #[test]
    fn evaluate_factorial() {
        let formula = Formula::new("3 !").unwrap();
        assert_eq!(formula.eval().unwrap(), Number(6.0.into()));
    }

    #[test]
    fn evaluate_sqrt() {
        let formula = Formula::new("sqrt(4)").unwrap();
        assert_eq!(formula.eval().unwrap(), Number(2.0.into()));
    }

    #[test]
    fn evaluate_abs() {
        let formula = Formula::new("abs(4)").unwrap();
        assert_eq!(formula.eval().unwrap(), Number(4.0.into()));

        let formula = Formula::new("abs(-4)").unwrap();
        assert_eq!(formula.eval().unwrap(), Number(4.0.into()));
    }

    #[test]
    fn evaluate_round() {
        let formula = Formula::new("round(1.0001, 2)").unwrap();
        assert_eq!(formula.eval().unwrap(), Number(1.0.into()));

        let formula = Formula::new("round(3.141592, 4)").unwrap();
        assert_eq!(formula.eval().unwrap(), Number(3.1416.into()));
    }

    #[test]
    fn evaluate_complex_expression() {
        let formula = Formula::new("sqrt(3*3 + (6/3+2)*4) - 1").unwrap();
        assert_eq!(formula.eval().unwrap(), Number(4.0.into()));
    }

    #[test]
    fn evaluate_var() {
        let vars: VariableDict = [("a", 1.0.into()), ("b", 2.0.into()), ("c", 3.0.into())]
            .iter()
            .map(|(k, v)| (k.to_owned().to_owned(), *v))
            .collect();
        let formula = Formula::new("a + b * c").unwrap();
        let result = formula.eval_with(&vars).unwrap();
        assert_eq!(result, Number(7.0.into()));
    }

    #[test]
    fn evaluate_literal() {
        let formula = Formula::new(r#" "Hello, World!" "#).unwrap();
        assert_eq!(formula.eval().unwrap(), Literal("Hello, World!".to_owned()));
    }

    #[test]
    fn evaluate_concatenation() {
        let formula = Formula::new(r#" "Hello, " + "World!" "#).unwrap();
        assert_eq!(formula.eval().unwrap(), Literal("Hello, World!".to_owned()));
    }

    #[test]
    fn evaluate_string_multiplication() {
        assert_eq!(
            Formula::new(r#" "a" * 4 "#).unwrap().eval().unwrap(),
            Literal("aaaa".to_owned())
        );
        assert_eq!(
            Formula::new(r#" 4 * "a" "#).unwrap().eval().unwrap(),
            Literal("aaaa".to_owned())
        );

        // too long strings cannot be produced
        assert!(Formula::new(r#" "a" * 4097 "#).unwrap().eval().is_err());
        assert!(Formula::new(r#" 4097 * "a" "#).unwrap().eval().is_err());
        // except all spaces
        assert!(Formula::new(r#" "" * 100! "#).unwrap().eval().is_ok());
        assert!(Formula::new(r#" 100! * "" "#).unwrap().eval().is_ok());
    }

    #[test]
    fn operation_viability() {
        assert!(Formula::new(r#"  1  +  1 "#).unwrap().eval().is_ok());
        assert!(Formula::new(r#"  1  + "a" "#).unwrap().eval().is_err());
        assert!(Formula::new(r#" "a" +  1 "#).unwrap().eval().is_err());
        assert!(Formula::new(r#" "a" + "a" "#).unwrap().eval().is_ok());

        assert!(Formula::new(r#"  1  -  1 "#).unwrap().eval().is_ok());
        assert!(Formula::new(r#"  1  - "a" "#).unwrap().eval().is_err());
        assert!(Formula::new(r#" "a" -  1 "#).unwrap().eval().is_err());
        assert!(Formula::new(r#" "a" - "a" "#).unwrap().eval().is_err());

        assert!(Formula::new(r#"  1  *  1 "#).unwrap().eval().is_ok());
        assert!(Formula::new(r#"  1  * "a" "#).unwrap().eval().is_ok());
        assert!(Formula::new(r#" "a" *  1 "#).unwrap().eval().is_ok());
        assert!(Formula::new(r#" "a" * "a" "#).unwrap().eval().is_err());

        assert!(Formula::new(r#"  1  /  1 "#).unwrap().eval().is_ok());
        assert!(Formula::new(r#"  1  / "a" "#).unwrap().eval().is_err());
        assert!(Formula::new(r#" "a" /  1 "#).unwrap().eval().is_err());
        assert!(Formula::new(r#" "a" / "a" "#).unwrap().eval().is_err());

        assert!(Formula::new(r#"  1  ^  1 "#).unwrap().eval().is_ok());
        assert!(Formula::new(r#"  1  ^ "a" "#).unwrap().eval().is_err());
        assert!(Formula::new(r#" "a" ^  1 "#).unwrap().eval().is_err());
        assert!(Formula::new(r#" "a" ^ "a" "#).unwrap().eval().is_err());

        assert!(Formula::new(r#" +  1  "#).unwrap().eval().is_ok());
        assert!(Formula::new(r#" + "a" "#).unwrap().eval().is_ok());

        assert!(Formula::new(r#" -  1  "#).unwrap().eval().is_ok());
        assert!(Formula::new(r#" - "a" "#).unwrap().eval().is_err());

        assert!(Formula::new(r#"  1 ! "#).unwrap().eval().is_ok());
        assert!(Formula::new(r#" "a"! "#).unwrap().eval().is_err());
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
                vars.insert(var, 0.0.into());
            }

            let formula = Formula::new(&infix_str).unwrap();
            let _ = formula.eval_with(&vars);
        }
    }
}
