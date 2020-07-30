use std::{cmp, fmt, ops, str};

use bigdecimal::{BigDecimal, FromPrimitive, One, ParseBigDecimalError, ToPrimitive};
use num::pow::pow;

use super::error::FormulaError;

pub type ParseError = ParseBigDecimalError;

macro_rules! build_err {
    ($($arg:tt)*) => {{
        FormulaError::NumericError(format!($($arg)*))
    }}
}

macro_rules! error {
    ($($arg:tt)*) => {{
        return Err(build_err!($($arg)*));
    }}
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Numeric {
    value: BigDecimal,
}

impl Numeric {
    pub fn abs(&self) -> Self {
        Self {
            value: self.value.abs(),
        }
    }

    pub fn trunc(&self) -> Self {
        Self {
            value: self.value.with_scale(0),
        }
    }

    pub fn round(&self, precision: &Self) -> Result<Self, FormulaError> {
        let precision = precision
            .value
            .to_usize()
            .ok_or(build_err!("precision is out of range"))?;
        if precision > 100 {
            error!("precision > 100 is not supported");
        }

        let scale = pow(BigDecimal::from(10), precision + 1);
        let pivot = BigDecimal::from(5) / scale;
        let pivoted = if self.value >= BigDecimal::from(0) {
            self.value.clone() + pivot
        } else {
            self.value.clone() - pivot
        };
        let rounded = pivoted.with_scale(precision as i64);
        Ok(Self { value: rounded })
    }

    pub fn sqrt(&self) -> Result<Self, FormulaError> {
        let sqrt = self
            .value
            .sqrt()
            .ok_or(build_err!("sqrt of negative numbers is not supported"))?;

        Ok(Self { value: sqrt })
    }

    pub fn pow(&self, rhs: &Self) -> Result<Self, FormulaError> {
        if rhs.abs().value > BigDecimal::from(100) {
            error!("exponents of magnitude > 100 are not supported");
        }

        let power = pow(
            self.value.clone(),
            rhs.value
                .abs()
                .to_usize()
                .ok_or(build_err!("the exponent is out of range"))?,
        );

        Ok(Self {
            value: if power == BigDecimal::from(0) || rhs.value >= BigDecimal::from(0) {
                power
            } else {
                BigDecimal::one() / power
            },
        })
    }

    pub fn factorial(&self) -> Result<Self, FormulaError> {
        if self.value > BigDecimal::from(100) {
            error!("factorial of values > 100 are not supported");
        }

        let max_index = self
            .value
            .to_usize()
            .ok_or(build_err!("factorial argument is out of range"))?;
        let factorial =
            (1..=max_index).fold(One::one(), |a, b| a * BigDecimal::from_usize(b).unwrap());

        Ok(Self { value: factorial })
    }
}

impl fmt::Display for Numeric {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // remove trailing decimal zeroes
        let representation = self.value.to_string();
        let representation = if representation.contains('.') {
            representation.trim_end_matches('0').trim_end_matches('.')
        } else {
            &representation
        };
        write!(f, "{}", representation)
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
        numeric.value.to_f64().unwrap()
    }
}

impl From<Numeric> for usize {
    fn from(numeric: Numeric) -> Self {
        numeric.value.to_usize().unwrap_or(std::usize::MAX)
    }
}

impl From<f64> for Numeric {
    fn from(value: f64) -> Self {
        Self {
            value: BigDecimal::from_f64(value).unwrap(),
        }
    }
}

impl cmp::PartialEq<f64> for Numeric {
    fn eq(&self, other: &f64) -> bool {
        self.value.eq(&BigDecimal::from_f64(other.clone()).unwrap())
    }
}

impl cmp::PartialOrd<f64> for Numeric {
    fn partial_cmp(&self, other: &f64) -> Option<cmp::Ordering> {
        self.value
            .partial_cmp(&BigDecimal::from_f64(other.clone()).unwrap())
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
    type Output = Result<Self, FormulaError>;
    fn div(self, rhs: Self) -> Self::Output {
        if rhs.value == BigDecimal::from(0) {
            error!("division by zero is not allowed");
        }
        Ok(Self {
            value: self.value / rhs.value,
        })
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
            Self {
                value: BigDecimal::from_f64(value.clone()).unwrap(),
            }
        }
    }

    impl From<u32> for Numeric {
        fn from(value: u32) -> Self {
            Self {
                value: BigDecimal::from_u32(value).unwrap(),
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
