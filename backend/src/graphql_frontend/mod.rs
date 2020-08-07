use juniper::{EmptyMutation, EmptySubscription, RootNode};

mod context;
mod query;

pub use context::Context;
pub use query::Query;

pub type Schema = RootNode<'static, Query, EmptyMutation<Context>, EmptySubscription<Context>>;

pub fn create_schema() -> Schema {
    Schema::new(
        Query,
        EmptyMutation::<Context>::new(),
        EmptySubscription::<Context>::new(),
    )
}
