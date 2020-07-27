use std::{cmp, fmt, str};

pub type ParseError = std::num::ParseFloatError;

#[derive(Debug, Clone, PartialEq)]
pub struct Numeric {
    value: f64,
}

impl Numeric {
    pub fn abs(&self) -> Self {
        Numeric {
            value: self.value.abs(),
        }
    }
}

impl fmt::Display for Numeric {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl str::FromStr for Numeric {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(Numeric { value: s.parse()? })
    }
}

// only used temporarily while migrating to `Numeric` in favor of `f64`
impl From<&Numeric> for f64 {
    fn from(Numeric: &Numeric) -> Self {
        Numeric.value
    }
}

impl From<f64> for Numeric {
    fn from(value: f64) -> Self {
        Numeric { value }
    }
}

impl cmp::PartialEq<f64> for Numeric {
    fn eq(&self, other: &f64) -> bool {
        self.value.eq(other)
    }
}

impl cmp::PartialOrd<f64> for Numeric {
    fn partial_cmp(&self, other: &f64) -> Option<cmp::Ordering> {
        self.value.partial_cmp(other)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    impl From<&f64> for Numeric {
        fn from(value: &f64) -> Self {
            Numeric { value: *value }
        }
    }

    impl From<u32> for Numeric {
        fn from(value: u32) -> Self {
            Numeric {
                value: value as f64,
            }
        }
    }

    impl Arbitrary for Numeric {
        type Parameters = ();
        type Strategy = BoxedStrategy<Self>;

        fn arbitrary_with(_: Self::Parameters) -> Self::Strategy {
            any::<f64>().prop_map(Self::from).boxed()
        }
    }
}
