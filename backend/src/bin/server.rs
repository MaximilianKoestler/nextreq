use juniper_rocket_async::{graphiql_source, GraphQLRequest, GraphQLResponse};
use rocket::http::Method;
use rocket::{response::content, State};
use rocket_cors::{self, AllowedHeaders, AllowedOrigins};
use std::error::Error;

use nextreq::graphql_frontend::{create_schema, Context, Schema};
use nextreq::interactors::{CalculatorInteractor, RequirementsInteractor};

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
async fn main() -> Result<(), Box<dyn Error>> {
    let cors = rocket_cors::CorsOptions {
        allowed_origins: AllowedOrigins::all(),
        allowed_methods: vec![Method::Post].into_iter().map(From::from).collect(),
        allowed_headers: AllowedHeaders::all(),
        allow_credentials: true,
        ..Default::default()
    }
    .to_cors()?;

    rocket::ignite()
        .manage(create_context())
        .manage(create_schema())
        .mount("/", rocket::routes![graphiql, post_graphql_handler])
        .attach(cors)
        .launch()
        .await?;

    Ok(())
}

fn create_context() -> Context {
    let calculator_interactor = Box::leak(Box::new(CalculatorInteractor::new()));
    let requirements_interactor = Box::leak(Box::new(RequirementsInteractor::new()));
    Context {
        calculator_view: calculator_interactor,
        requirements_view: requirements_interactor,
    }
}
