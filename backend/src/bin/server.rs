use juniper_rocket_async::{graphiql_source, GraphQLRequest, GraphQLResponse};
use rocket::http::Method;
use rocket::{response::content, State};
use rocket_contrib::serve::StaticFiles;
use rocket_cors::{self, AllowedHeaders, AllowedOrigins};
use std::env;
use std::error::Error;

use nextreq::graphql_frontend::{create_schema, Context, Schema};
use nextreq::interactors::{CalculatorInteractor, RequirementsInteractor};

#[rocket::get("/")]
fn graphiql() -> content::Html<String> {
    graphiql_source("/graphql")
}

#[rocket::post("/", data = "<request>")]
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

    let mut rocket = rocket::ignite()
        .manage(create_context())
        .manage(create_schema())
        .mount("/graphql", rocket::routes![post_graphql_handler]);

    // the graphiql editor either gets mounted to / (no static files present) or to /graphiql
    if let Ok(path) = env::var("WWW_ROOT") {
        rocket = rocket
            .mount("/graphiql", rocket::routes![graphiql])
            .mount("/", StaticFiles::from(path));
    } else {
        rocket = rocket.mount("/", rocket::routes![graphiql])
    }

    rocket.attach(cors).launch().await?;

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
