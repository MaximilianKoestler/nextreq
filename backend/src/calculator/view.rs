pub trait CalculatorView: Send + Sync + 'static {
    fn calculate(&self, input: &str) -> Result<String, String>;
}
