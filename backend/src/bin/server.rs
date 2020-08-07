use juniper_rocket_async::{graphiql_source, GraphQLRequest, GraphQLResponse};
use rocket::{response::content, State};

use nextreq::graphql_frontend::{create_context, create_schema, Context, Schema};

#[rocket::get("/")]
fn graphiql() -> content::Html<String> {
    graphiql_source("/graphql")
}

#[rocket::post("/graphql", data = "<request>")]
fn post_graphql_handler(
    schema: State<Schema>,
    context: State<Context>,
    request: GraphQLRequest,
) -> GraphQLResponse {
    request.execute_sync(&schema, &context)
}

#[rocket::main]
async fn main() -> Result<(), rocket::error::Error> {
    rocket::ignite()
        .manage(create_context())
        .manage(create_schema())
        .mount("/", rocket::routes![graphiql, post_graphql_handler])
        .launch()
        .await
}
