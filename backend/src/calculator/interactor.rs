use super::model::CalculatorError;
use super::view::CalculatorView;

use crate::formula::Formula;

pub struct CalculatorInteractor {}

impl CalculatorInteractor {
    pub fn new() -> Self {
        Self {}
    }
}

impl CalculatorView for CalculatorInteractor {
    fn calculate(&self, input: &str) -> Result<String, CalculatorError> {
        let formula = Formula::new(input);
        let result = formula.and_then(|f| f.eval());

        result.map(|v| v.to_string()).map_err(|e| CalculatorError {
            message: e.error.to_string(),
            start: e.offset,
            end: if e.offset == -1 { -1 } else { e.offset + 1 },
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn calculate_simple_formula() {
        let calculator = CalculatorInteractor::new();
        let calculator: Box<dyn CalculatorView> = Box::new(calculator);

        let result = calculator.calculate("1 + 1").unwrap();
        assert_eq!(result, "2");
    }

    #[test]
    fn calculate_with_error() {
        let calculator = CalculatorInteractor::new();
        let calculator: Box<dyn CalculatorView> = Box::new(calculator);

        let result = calculator.calculate(")");
        assert!(result.is_err());
    }
}
