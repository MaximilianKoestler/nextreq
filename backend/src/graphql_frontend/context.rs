use crate::calculator::CalculatorView;
use crate::requirements::RequirementsView;

#[derive(Clone, Copy)]
pub struct Context {
    pub calculator_view: &'static dyn CalculatorView,
    pub requirements_view: &'static dyn RequirementsView,
}

impl juniper::Context for Context {}
