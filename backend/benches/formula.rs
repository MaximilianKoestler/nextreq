use criterion::{black_box, criterion_group, criterion_main, Criterion};
use itertools::Itertools;
use std::iter::repeat;

use nextreq::formula;

fn benchmark_many_additions(c: &mut Criterion) {
    let n = 10000;
    let input = repeat("+1").take(n).join("");

    c.bench_function(&format!("parse/evaluate additions ({})", n), |b| {
        b.iter(|| {
            formula::Formula::new(black_box(&input))
                .unwrap()
                .eval()
                .unwrap()
        })
    });

    c.bench_function(&format!("evaluate additions ({})", n), |b| {
        let formula = formula::Formula::new(black_box(&input)).unwrap();
        b.iter(|| formula.eval().unwrap())
    });
}

fn benchmark_example_term(c: &mut Criterion) {
    let input = "sqrt(3*3 + (6/3+2)*4) - 1";

    c.bench_function(&format!("parse/evaluate example"), |b| {
        b.iter(|| {
            formula::Formula::new(black_box(&input))
                .unwrap()
                .eval()
                .unwrap()
        })
    });

    c.bench_function(&format!("evaluate example"), |b| {
        let formula = formula::Formula::new(black_box(&input)).unwrap();
        b.iter(|| formula.eval().unwrap())
    });
}

criterion_group! {
    name = benches;
    config = Criterion::default();
    targets = benchmark_many_additions,
              benchmark_example_term,
}
criterion_main!(benches);
