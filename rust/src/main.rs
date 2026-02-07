mod parser;

use actix_cors::Cors;
use actix_web::{web, App, HttpResponse, HttpServer, Responder};
use parser::types::{QueryResult, Row};
use parser::SQLParser;
use serde::Deserialize;
use std::env;

#[derive(Deserialize)]
struct ExecuteRequest {
    query: String,
    data: Vec<Row>,
}

async fn execute_query(req: web::Json<ExecuteRequest>) -> impl Responder {
    let parser = SQLParser::new(req.data.clone());

    let parsed = match parser.parse(&req.query) {
        Ok(q) => q,
        Err(e) => {
            return HttpResponse::Ok().json(QueryResult {
                success: false,
                data: None,
                error: Some(e),
            });
        }
    };

    match parser.execute(&parsed) {
        Ok(results) => HttpResponse::Ok().json(QueryResult {
            success: true,
            data: Some(results),
            error: None,
        }),
        Err(e) => HttpResponse::Ok().json(QueryResult {
            success: false,
            data: None,
            error: Some(e),
        }),
    }
}

async fn health() -> impl Responder {
    HttpResponse::Ok().json(serde_json::json!({"status": "ok"}))
}

#[actix_web::main]
async fn main() -> std::io::Result<()> {
    env_logger::init();

    let port: u16 = env::var("PORT")
        .unwrap_or_else(|_| "8081".to_string())
        .parse()
        .expect("PORT must be a number");

    println!("Rust SQL Parser server starting on port {}", port);

    HttpServer::new(|| {
        let cors = Cors::default()
            .allow_any_origin()
            .allow_any_method()
            .allow_any_header();

        App::new()
            .wrap(cors)
            .route("/execute", web::post().to(execute_query))
            .route("/health", web::get().to(health))
    })
    .bind(("0.0.0.0", port))?
    .run()
    .await
}
