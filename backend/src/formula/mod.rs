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
use std::time::{Duration, Instant};

use error::{FormulaError, PositionedFormulaError};
use numeric::Numeric;
use quoted_string::Quotable;

pub struct Formula {
    parser: parser::Parser,
}

macro_rules! take {
    ($stack:ident) => {
        ($stack)
            .pop()
            .ok_or(FormulaError::EvaluationError(
                "expected value, found empty stack".to_owned(),
            ))
            .map_err(|e| e.at(0))?;
    };
}

macro_rules! error {
    ($($arg:tt)*) => {{
        return Err(FormulaError::EvaluationError(format!($($arg)*)))
    }}
}

macro_rules! error2 {
    ($offset:expr, $($arg:tt)*) => {{
        return Err(FormulaError::EvaluationError(format!($($arg)*)).at($offset as isize))
    }}
}

macro_rules! enforce_number {
    ($description:literal, $var:ident) => {
        match $var {
            Number(value) => value,
            Literal(_) => error2!(
                0,
                "the {} is not supported for string literals",
                $description
            ),
        }
    };

    ($description:literal, $var1:ident, $var2:ident) => {
        (
            match $var1 {
                Number(value) => value,
                Literal(_) => error2!(
                    0,
                    "the {} is not supported for string literals",
                    $description
                ),
            },
            match $var2 {
                Number(value) => value,
                Literal(_) => error2!(
                    0,
                    "the {} is not supported for string literals",
                    $description
                ),
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
    pub fn new(input: &str) -> Result<Self, PositionedFormulaError> {
        let lexer = lexer::Lexer::new(input)?;
        let parser = parser::Parser::new(&lexer[..])?;
        Ok(Self { parser })
    }

    pub fn eval(&self) -> Result<Value, PositionedFormulaError> {
        self.eval_internal(None)
    }

    pub fn eval_with(&self, vars: &VariableDict) -> Result<Value, PositionedFormulaError> {
        self.eval_internal(Some(vars))
    }

    fn eval_internal(&self, vars: Option<&VariableDict>) -> Result<Value, PositionedFormulaError> {
        // TODO: move to lazy_static block or somewhere else
        let global_constants = {
            use std::f64::consts;

            let mut map = VariableDict::new();
            map.insert("PI".to_owned(), consts::PI.into());
            map.insert("E".to_owned(), consts::E.into());
            map
        };

        let start = Instant::now();
        let timeout = Duration::from_millis(500);

        let mut stack: Vec<Value> = vec![];
        for item in self.parser.iter() {
            if start.elapsed() > timeout {
                error2!(
                    0,
                    "timeout exceeded ({:?} out of {:?})",
                    start.elapsed(),
                    timeout
                );
            }

            match item {
                parser::ParseItem::Value(v) => match v {
                    parser::Value::Number(value) => stack.push(Number(value.clone())),
                    parser::Value::Literal(text) => stack.push(Literal(text.clone())),
                    parser::Value::Variable(name) => {
                        let var = vars
                            .as_ref()
                            .map(|m| m.get(name))
                            .flatten()
                            .or(global_constants.get(name))
                            .ok_or(FormulaError::EvaluationError(format!(
                                "variable '{}' not found",
                                name
                            )))
                            .map_err(|e| e.at(0))?
                            .clone();
                        stack.push(Number(var));
                    }
                },
                parser::ParseItem::Operator(op) => match op {
                    parser::Operator::Add => {
                        let rhs = take!(stack);
                        let lhs = take!(stack);
                        stack.push((lhs + rhs).map_err(|e| e.at(0))?);
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
                        stack.push((lhs * rhs).map_err(|e| e.at(0))?);
                    }
                    parser::Operator::Div => {
                        let rhs = take!(stack);
                        let lhs = take!(stack);
                        let (rhs, lhs) = enforce_number!("division operator", rhs, lhs);
                        stack.push(Number((lhs / rhs).map_err(|e| e.at(0))?));
                    }
                    parser::Operator::Pow => {
                        let rhs = take!(stack);
                        let lhs = take!(stack);
                        let (rhs, lhs) = enforce_number!("power operator", rhs, lhs);

                        stack.push(Number(lhs.pow(&rhs).map_err(|e| e.at(0))?));
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
                        stack.push(Number(lhs.factorial().map_err(|e| e.at(0))?));
                    }
                },
                parser::ParseItem::Function(f, _) => match f {
                    parser::Function::Sqrt => {
                        let param = take!(stack);
                        let param = enforce_number!("sqrt function", param);
                        stack.push(Number(param.sqrt().map_err(|e| e.at(0))?));
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
                        stack.push(Number(value.round(&precision).map_err(|e| e.at(0))?));
                    }
                },
            }
        }

        if stack.len() != 1 {
            Err(FormulaError::EvaluationError(format!(
                "expected exactly one item, found {}",
                stack.len()
            ))
            .at(0))
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

        let formula = Formula::new("2 ^ -2").unwrap();
        assert_eq!(formula.eval().unwrap(), Number(0.25.into()));
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

        let formula = Formula::new("sqrt(-4)").unwrap();
        assert!(formula.eval().is_err());
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

        let formula = Formula::new("round(3.241592, 4)").unwrap();
        assert_eq!(formula.eval().unwrap(), Number(3.2416.into()));

        let formula = Formula::new("round(-1.551, 1)").unwrap();
        assert_eq!(formula.eval().unwrap(), Number((-1.6).into()));
    }

    #[test]
    fn evaluate_complex_expression() {
        let formula = Formula::new("sqrt(3*3 + (6/3+2)*4) - 1").unwrap();
        assert_eq!(formula.eval().unwrap(), Number(4.0.into()));
    }

    #[test]
    fn evaluate_var() {
        let mut vars: VariableDict = VariableDict::new();
        vars.insert("a".to_owned(), 1.0.into());
        vars.insert("b".to_owned(), 2.0.into());
        vars.insert("c".to_owned(), 3.0.into());

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

    #[test]
    fn global_constants() {
        let rounded = move |v| Number(Numeric::from(v).round(&4.into()).unwrap());

        assert_eq!(
            Formula::new("round(PI, 4)").unwrap().eval().unwrap(),
            rounded(std::f64::consts::PI)
        );
        assert_eq!(
            Formula::new("round(E, 4)").unwrap().eval().unwrap(),
            rounded(std::f64::consts::E)
        );
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

    #[test]
    #[ignore]
    fn expression_performance_bound() {
        let token_strategy = any::<parser::tests::TokenTree>();
        let mut runner = proptest::test_runner::TestRunner::default();

        let mut results = vec![];
        for _ in 0..4096 {
            let token_tree = token_strategy.new_tree(&mut runner).unwrap().current();
            let infix_str = token_tree.infix_str();
            let formula = Formula::new(&infix_str).unwrap();

            let mut vars = VariableDict::new();
            for var in token_tree.variables() {
                vars.insert(var, 0.0.into());
            }

            let start = Instant::now();
            let _ = formula.eval_with(&vars);
            let duration = start.elapsed();

            let score = duration.as_nanos() / infix_str.len() as u128;
            results.push((infix_str, duration, score));
        }

        results.sort_by_key(|(_, _, score)| *score);
        let sample_size = 10;

        println!("\nSlowest {}:", sample_size);
        for (index, top) in results.iter().rev().take(sample_size).enumerate() {
            let (infix, duration, _) = top;
            println!("{:3}: {:?} <- {:?}", index + 1, duration, infix);
        }
    }
}
