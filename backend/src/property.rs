use std::fmt;

use crate::formula::{error::FormulaError, Formula};

#[derive(Debug, Clone)]
pub enum PropertyError {
    FormulaError(FormulaError),
    NoMatchError(String),
}

impl fmt::Display for PropertyError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Self::FormulaError(err) => write!(f, "{}", err),
            Self::NoMatchError(err) => write!(f, "{}", err),
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

pub enum Match {
    Equal(f64),
    Range(f64, f64),
    Any,
}

pub struct TableProperty {
    input: Box<dyn Property>,
    matches: Vec<(Match, Box<dyn Property>)>,
}

impl TableProperty {
    fn new(input: Box<dyn Property>, matches: Vec<(Match, Box<dyn Property>)>) -> Self {
        Self {
            input: input,
            matches: matches,
        }
    }
}

impl Property for TableProperty {
    fn value(&self) -> Result<f64, PropertyError> {
        let value = self.input.value()?;
        for (m, prop) in &self.matches {
            match m {
                Match::Equal(v) if value == *v => {
                    return prop.value();
                }
                Match::Range(min, max) if value >= *min && value <= *max => {
                    return prop.value();
                }
                Match::Any => {
                    return prop.value();
                }
                _ => {}
            }
        }
        Err(PropertyError::NoMatchError(format!(
            "value {} could not be matched",
            value
        )))
    }
    fn unit(&self) -> Option<&str> {
        todo!()
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
    fn formula_property_addition() {
        let p = FormulaProperty::new("1 + 2", Some("m")).unwrap();
        assert_eq!(p.value().unwrap(), 3.0);
    }

    proptest! {
        #![proptest_config(ProptestConfig {
            cases: 1024,
            .. ProptestConfig::default()
        })]
        #[test]
        fn formula_property_does_not_crash(input: String) {
            let p = FormulaProperty::new(&input, Some("m"));

            if p.is_ok() {
                let _ = p.unwrap().value();
            }
        }
    }

    #[test]
    fn table_property_no_match() {
        let input = ValueProperty::new(1.0, Some("m"));
        let matches = vec![(
            Match::Equal(2.0),
            Box::new(ValueProperty::new(4.0, Some("m"))) as Box<dyn Property>,
        )];
        let p = TableProperty::new(Box::new(input), matches);
        assert!(p.value().is_err());
    }

    #[test]
    fn table_property_equals() {
        let input = ValueProperty::new(2.0, Some("m"));
        let matches = vec![(
            Match::Equal(2.0),
            Box::new(ValueProperty::new(4.0, Some("m"))) as Box<dyn Property>,
        )];
        let p = TableProperty::new(Box::new(input), matches);
        assert_eq!(p.value().unwrap(), 4.0);
    }

    #[test]
    fn table_property_range() {
        let input = ValueProperty::new(2.5, Some("m"));
        let matches = vec![(
            Match::Range(2.0, 3.0),
            Box::new(ValueProperty::new(4.0, Some("m"))) as Box<dyn Property>,
        )];
        let p = TableProperty::new(Box::new(input), matches);
        assert_eq!(p.value().unwrap(), 4.0);
    }

    #[test]
    fn table_property_any() {
        let input = ValueProperty::new(0.0, Some("m"));
        let matches = vec![(
            Match::Any,
            Box::new(ValueProperty::new(4.0, Some("m"))) as Box<dyn Property>,
        )];
        let p = TableProperty::new(Box::new(input), matches);
        assert_eq!(p.value().unwrap(), 4.0);
    }

    #[test]
    fn table_property_complex() {
        let input = FormulaProperty::new("3!", Some("m")).unwrap();
        let matches = vec![
            (
                Match::Equal(1.0),
                Box::new(ValueProperty::new(4.0, Some("m"))) as Box<dyn Property>,
            ),
            (
                Match::Range(2.0, 7.0),
                Box::new(FormulaProperty::new("5 + 1", Some("m")).unwrap()) as Box<dyn Property>,
            ),
            (
                Match::Any,
                Box::new(ValueProperty::new(2.0, Some("m"))) as Box<dyn Property>,
            ),
        ];
        let p = TableProperty::new(Box::new(input), matches);
        assert_eq!(p.value().unwrap(), 6.0);
    }
}
