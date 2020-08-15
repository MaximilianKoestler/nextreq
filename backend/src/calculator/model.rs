#[derive(Debug)]
pub struct CalculatorError {
    pub message: String,
    pub start: isize,
    pub end: isize,
}
