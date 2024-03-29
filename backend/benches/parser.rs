use criterion::{black_box, criterion_group, criterion_main, Criterion};
use std::iter::repeat;

use nextreq::formula::lexer::{Operator, PositionedToken, Token};
use nextreq::formula::parser;

fn benchmark_many_additions(c: &mut Criterion) {
    let n = 10000;

    let part = vec![
        Token::Operator(Operator::Plus),
        Token::Number(std::f64::consts::PI.into()),
    ];
    let input: Vec<PositionedToken> = repeat(part.iter())
        .take(n)
        .flatten()
        .cloned()
        .map(|t| t.at(0, 0))
        .collect();

    c.bench_function(&format!("parse additions ({})", n), |b| {
        b.iter(|| parser::Parser::new(black_box(&input)).unwrap())
    });
}

criterion_group! {
    name = benches;
    config = Criterion::default();
    targets = benchmark_many_additions,
}
criterion_main!(benches);
