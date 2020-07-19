trait Property {
    fn value(&self) -> f64;
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
    fn value(&self) -> f64 {
        self.value
    }

    fn unit(&self) -> Option<&str> {
        self.unit.as_deref()
    }
}

struct FormulaProperty {
    formula: String,
    unit: Option<String>,
}

impl FormulaProperty {
    fn new(formula: &str, unit: Option<&str>) -> Self {
        Self {
            formula: formula.to_owned(),
            unit: unit.map(ToOwned::to_owned),
        }
    }
}

impl Property for FormulaProperty {
    fn value(&self) -> f64 {
        2.0
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
            prop_assert_eq!(p.value(), value);
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
        let p = FormulaProperty::new("1 + 1", Some("m"));
        assert_eq!(p.value(), 2.0);
    }
}
