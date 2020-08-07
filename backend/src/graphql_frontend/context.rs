use crate::calculator::CalculatorView;

#[derive(Clone, Copy)]
pub struct Context {
    pub calculator_view: &'static dyn CalculatorView,
}

impl juniper::Context for Context {}
