use std::fmt;

use crate::formula::{error::FormulaError, Formula, Value, Value::Literal, Value::Number};

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
    fn value(&self) -> Result<Value, PropertyError>;
    fn unit(&self) -> Option<&str>;
}

type PropBox = Box<dyn Property>;

struct ValueProperty {
    value: Value,
    unit: Option<String>,
}

impl ValueProperty {
    fn new(value: Value, unit: Option<&str>) -> Self {
        Self {
            value,
            unit: unit.map(ToOwned::to_owned),
        }
    }
}

impl Property for ValueProperty {
    fn value(&self) -> Result<Value, PropertyError> {
        Ok(self.value.clone())
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
    fn value(&self) -> Result<Value, PropertyError> {
        self.formula.eval().map_err(PropertyError::FormulaError)
    }

    fn unit(&self) -> Option<&str> {
        self.unit.as_deref()
    }
}

pub enum Match {
    Equal(PropBox),
    Range(PropBox, PropBox),
    Any,
}

pub struct TableProperty {
    input: PropBox,
    matches: Vec<(Match, PropBox)>,
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
    fn value(&self) -> Result<Value, PropertyError> {
        let value = self.input.value()?;
        for (m, prop) in &self.matches {
            match m {
                Match::Equal(v) if value == v.value()? => {
                    return prop.value();
                }
                Match::Range(min, max) if value >= min.value()? && value <= max.value()? => {
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
    use crate::formula::numeric::Numeric;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn value_property_keeps_value(value: Numeric, unit: Option<String>) {
            let p = ValueProperty::new(Number(value), unit.as_deref());
            prop_assert_eq!(p.value().unwrap(), Number(value));
        }
    }

    proptest! {
        #[test]
        fn value_property_keeps_unit(value: Numeric, unit: Option<String>) {
            let unit = unit.as_deref();
            let p = ValueProperty::new(Number(value), unit);
            prop_assert_eq!(p.unit(), unit);
        }
    }

    #[test]
    fn formula_property_addition() {
        let p = FormulaProperty::new("1 + 2", Some("m")).unwrap();
        assert_eq!(p.value().unwrap(), Number(3.0.into()));
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
        let input = ValueProperty::new(Number(1.0.into()), Some("m"));
        let matches = vec![(
            Match::Equal(Box::new(ValueProperty::new(Number(2.0.into()), Some("m")))),
            Box::new(ValueProperty::new(Number(4.0.into()), Some("m"))) as Box<dyn Property>,
        )];
        let p = TableProperty::new(Box::new(input), matches);
        assert!(p.value().is_err());
    }

    #[test]
    fn table_property_equals() {
        let input = ValueProperty::new(Number(2.0.into()), Some("m"));
        let matches = vec![(
            Match::Equal(Box::new(ValueProperty::new(Number(2.0.into()), Some("m")))),
            Box::new(ValueProperty::new(Number(4.0.into()), Some("m"))) as Box<dyn Property>,
        )];
        let p = TableProperty::new(Box::new(input), matches);
        assert_eq!(p.value().unwrap(), Number(4.0.into()));
    }

    #[test]
    fn table_property_range() {
        let input = ValueProperty::new(Number(2.5.into()), Some("m"));
        let matches = vec![(
            Match::Range(
                Box::new(ValueProperty::new(Number(2.0.into()), Some("m"))),
                Box::new(ValueProperty::new(Number(3.0.into()), Some("m"))),
            ),
            Box::new(ValueProperty::new(Number(4.0.into()), Some("m"))) as Box<dyn Property>,
        )];
        let p = TableProperty::new(Box::new(input), matches);
        assert_eq!(p.value().unwrap(), Number(4.0.into()));
    }

    #[test]
    fn table_property_any() {
        let input = ValueProperty::new(Number(0.0.into()), Some("m"));
        let matches = vec![(
            Match::Any,
            Box::new(ValueProperty::new(Number(4.0.into()), Some("m"))) as Box<dyn Property>,
        )];
        let p = TableProperty::new(Box::new(input), matches);
        assert_eq!(p.value().unwrap(), Number(4.0.into()));
    }

    #[test]
    fn table_property_complex() {
        let input = FormulaProperty::new("3!", Some("m")).unwrap();
        let matches = vec![
            (
                Match::Equal(Box::new(ValueProperty::new(Number(1.0.into()), Some("m")))),
                Box::new(ValueProperty::new(Number(4.0.into()), Some("m"))) as Box<dyn Property>,
            ),
            (
                Match::Range(
                    Box::new(ValueProperty::new(Number(2.0.into()), Some("m"))),
                    Box::new(ValueProperty::new(Number(7.0.into()), Some("m"))),
                ),
                Box::new(FormulaProperty::new("5 + 1", Some("m")).unwrap()) as Box<dyn Property>,
            ),
            (
                Match::Any,
                Box::new(ValueProperty::new(Number(2.0.into()), Some("m"))) as Box<dyn Property>,
            ),
        ];
        let p = TableProperty::new(Box::new(input), matches);
        assert_eq!(p.value().unwrap(), Number(6.0.into()));
    }
}
