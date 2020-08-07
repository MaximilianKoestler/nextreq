use super::Context;

use juniper::graphql_object;

#[derive(Clone, Copy, Debug)]
pub struct Query;

#[graphql_object(Context = Context)]
impl Query {
    fn test() -> i32 {
        1
    }
}
