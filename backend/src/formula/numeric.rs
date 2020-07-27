use std::{cmp, fmt, ops, str};

pub type ParseError = std::num::ParseFloatError;

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Numeric {
    value: f64,
}

impl Numeric {
    pub fn abs(&self) -> Self {
        Self {
            value: self.value.abs(),
        }
    }

    pub fn trunc(&self) -> Self {
        Self {
            value: self.value.trunc(),
        }
    }

    pub fn round(&self) -> Self {
        Self {
            value: self.value.round(),
        }
    }

    pub fn sqrt(&self) -> Self {
        Self {
            value: self.value.sqrt(),
        }
    }

    pub fn pow(&self, rhs: &Self) -> Self {
        Self {
            value: self.value.powf(rhs.value),
        }
    }

    pub fn factorial(&self) -> Self {
        // all factorials larger than `170!` will overflow an `f64`
        Self {
            value: match self.value {
                v if v < 0.0 => std::f64::NAN,
                v if v > 170.0 => std::f64::INFINITY,
                v => (1..=(v as u32)).fold(1.0, |a, b| a * b as f64),
            },
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
        Ok(Self { value: s.parse()? })
    }
}

// only used temporarily while migrating to `Numeric` in favor of `f64`
impl From<&Numeric> for f64 {
    fn from(numeric: &Numeric) -> Self {
        numeric.value
    }
}

impl From<Numeric> for usize {
    fn from(numeric: Numeric) -> Self {
        numeric.value as usize
    }
}

impl From<f64> for Numeric {
    fn from(value: f64) -> Self {
        Self { value }
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

impl ops::Add for Numeric {
    type Output = Self;
    fn add(self, rhs: Self) -> Self::Output {
        Self {
            value: self.value + rhs.value,
        }
    }
}

impl ops::Sub for Numeric {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self::Output {
        Self {
            value: self.value - rhs.value,
        }
    }
}

impl ops::Mul for Numeric {
    type Output = Self;
    fn mul(self, rhs: Self) -> Self::Output {
        Self {
            value: self.value * rhs.value,
        }
    }
}

impl ops::Div for Numeric {
    type Output = Self;
    fn div(self, rhs: Self) -> Self::Output {
        Self {
            value: self.value / rhs.value,
        }
    }
}

impl ops::Neg for Numeric {
    type Output = Self;
    fn neg(self) -> Self::Output {
        Self { value: -self.value }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use proptest::prelude::*;

    impl From<&f64> for Numeric {
        fn from(value: &f64) -> Self {
            Self { value: *value }
        }
    }

    impl From<u32> for Numeric {
        fn from(value: u32) -> Self {
            Self {
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
