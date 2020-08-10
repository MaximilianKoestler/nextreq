use juniper_rocket_async::{graphiql_source, GraphQLRequest, GraphQLResponse};
use rocket::http::{self, Header, Method};
use rocket::{
    fairing::{Fairing, Info, Kind},
    response::content,
    Request, Response, State,
};

use nextreq::graphql_frontend::{create_schema, Context, Schema};
use nextreq::interactors::CalculatorInteractor;

#[rocket::get("/")]
fn graphiql() -> content::Html<String> {
    graphiql_source("/graphql")
}

#[rocket::options("/graphql")]
fn cors_preflight_handler<'a>() -> Response<'a> {
    Response::build()
        .raw_header("Access-Control-Allow-Origin", "*")
        .raw_header("Access-Control-Allow-Methods", "OPTIONS, POST")
        .raw_header("Access-Control-Allow-Headers", "Content-Type")
        .finalize()
}

#[rocket::post("/graphql", data = "<request>")]
fn post_graphql_handler(
    schema: State<Schema>,
    context: State<Context>,
    request: GraphQLRequest,
) -> GraphQLResponse {
    request.execute_sync(&schema, &context)
}

struct AllowOriginsFairing;

#[rocket::async_trait]
impl Fairing for AllowOriginsFairing {
    fn info(&self) -> rocket::fairing::Info {
        Info {
            name: "Add Access-Control-Allow-Origin header",
            kind: Kind::Response,
        }
    }

    async fn on_response<'r>(&self, req: &'r Request<'_>, res: &mut Response<'r>) {
        if req.method() == Method::Post {
            res.adjoin_header(Header::new(
                http::hyper::header::ACCESS_CONTROL_ALLOW_ORIGIN.as_str(),
                "*",
            ))
        }
    }
}

#[rocket::main]
async fn main() -> Result<(), rocket::error::Error> {
    rocket::ignite()
        .manage(create_context())
        .manage(create_schema())
        .mount(
            "/",
            rocket::routes![graphiql, post_graphql_handler, cors_preflight_handler],
        )
        .attach(AllowOriginsFairing {})
        .launch()
        .await
}

fn create_context() -> Context {
    let calculator_interactor = Box::leak(Box::new(CalculatorInteractor::new()));
    Context {
        calculator_view: calculator_interactor,
    }
}
