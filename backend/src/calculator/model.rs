#[derive(Debug)]
pub struct CalculatorError {
    pub message: String,
    pub offset: isize,
}
