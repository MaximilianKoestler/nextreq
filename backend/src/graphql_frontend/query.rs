use super::Context;

use juniper::{graphql_object, FieldResult};

#[derive(Clone, Copy, Debug)]
pub struct Query;

#[graphql_object(Context = Context)]
impl Query {
    fn calculate(input: String, context: &Context) -> FieldResult<String> {
        context
            .calculator_view
            .calculate(&input)
            .map_err(|e| e.into())
    }
}
