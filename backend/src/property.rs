use std::fmt;

use crate::formula::{error::FormulaError, Formula};

#[derive(Debug, Clone)]
pub enum PropertyError {
    FormulaError(FormulaError),
}

impl fmt::Display for PropertyError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::FormulaError(err) => write!(f, "{}", err),
        }
    }
}

pub trait Property {
    fn value(&self) -> Result<f64, PropertyError>;
    fn unit(&self) -> Option<&str>;
}

struct ValueProperty {
    value: f64,
    unit: Option<String>,
}

impl ValueProperty {
    fn new(value: f64, unit: Option<&str>) -> Self {
        Self {
            value,
            unit: unit.map(ToOwned::to_owned),
        }
    }
}

impl Property for ValueProperty {
    fn value(&self) -> Result<f64, PropertyError> {
        Ok(self.value)
    }

    fn unit(&self) -> Option<&str> {
        self.unit.as_deref()
    }
}

pub struct FormulaProperty {
    formula: Formula,
    unit: Option<String>,
}

impl FormulaProperty {
    pub fn new(formula: &str, unit: Option<&str>) -> Result<Self, FormulaError> {
        Ok(Self {
            formula: Formula::new(formula)?,
            unit: unit.map(ToOwned::to_owned),
        })
    }
}

impl Property for FormulaProperty {
    fn value(&self) -> Result<f64, PropertyError> {
        self.formula.eval().map_err(PropertyError::FormulaError)
    }

    fn unit(&self) -> Option<&str> {
        self.unit.as_deref()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn value_property_keeps_value(value: f64, unit: Option<String>) {
            let p = ValueProperty::new(value, unit.as_deref());
            prop_assert_eq!(p.value().unwrap(), value);
        }
    }

    proptest! {
        #[test]
        fn value_property_keeps_unit(value: f64, unit: Option<String>) {
            let unit = unit.as_deref();
            let p = ValueProperty::new(value, unit);
            prop_assert_eq!(p.unit(), unit);
        }
    }

    #[test]
    fn formula_addition() {
        let p = FormulaProperty::new("1 + 2", Some("m")).unwrap();
        assert_eq!(p.value().unwrap(), 3.0);
    }

    proptest! {
        #![proptest_config(ProptestConfig {
            cases: 1024,
            .. ProptestConfig::default()
        })]
        #[test]
        fn formula_does_not_crash(input: String) {
            let p = FormulaProperty::new(&input, Some("m"));

            if p.is_ok() {
                let _ = p.unwrap().value();
            }
        }
    }
}
