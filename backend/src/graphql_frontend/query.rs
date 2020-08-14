use juniper::{graphql_object, graphql_value, FieldError, FieldResult};

use super::Context;

#[derive(Clone, Copy, Debug)]
pub struct Query;

#[graphql_object(Context = Context)]
impl Query {
    fn calculate(input: String, context: &Context) -> FieldResult<String> {
        context.calculator_view.calculate(&input).map_err(|e| {
            FieldError::new(e.message, graphql_value!({ "offset": (e.offset as i32) }))
        })
    }
}
