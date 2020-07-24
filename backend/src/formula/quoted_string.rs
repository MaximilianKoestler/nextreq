pub trait Quotable {
    fn quote(&self) -> String;
}

impl Quotable for String {
    fn quote(&self) -> String {
        format!("\"{}\"", self)
    }
}
