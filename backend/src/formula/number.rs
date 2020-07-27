use std::{cmp, fmt, str};

pub type ParseError = std::num::ParseFloatError;

#[derive(Debug, Clone, PartialEq)]
pub struct Number {
    value: f64,
}

impl Number {
    pub fn abs(&self) -> Self {
        Number {
            value: self.value.abs(),
        }
    }
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl str::FromStr for Number {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Number { value: s.parse()? })
    }
}

// only used temporarily while migrating to `Number` in favor of `f64`
impl From<&Number> for f64 {
    fn from(number: &Number) -> Self {
        number.value
    }
}

impl cmp::PartialEq<f64> for Number {
    fn eq(&self, other: &f64) -> bool {
        self.value.eq(other)
    }
}

impl cmp::PartialOrd<f64> for Number {
    fn partial_cmp(&self, other: &f64) -> Option<cmp::Ordering> {
        self.value.partial_cmp(other)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    impl From<f64> for Number {
        fn from(value: f64) -> Self {
            Number { value }
        }
    }

    impl From<&f64> for Number {
        fn from(value: &f64) -> Self {
            Number { value: *value }
        }
    }

    impl Arbitrary for Number {
        type Parameters = ();
        type Strategy = BoxedStrategy<Self>;

        fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
            any::<f64>().prop_map(Self::from).boxed()
        }
    }
}
