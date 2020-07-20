use criterion::{black_box, criterion_group, criterion_main, Criterion};
use itertools::Itertools;
use rand::{distributions::Alphanumeric, Rng};
use std::iter::{once, repeat};

use nextreq::formula::lexer;

fn benchmark_long_identifier(c: &mut Criterion) {
    let len = 10000;

    let input: String = repeat("a").take(len).collect();

    c.bench_function(&format!("lex identifier (1 x {})", len), |b| {
        b.iter(|| lexer::Lexer::new(black_box(&input)))
    });
}

fn benchmark_many_identifiers(c: &mut Criterion) {
    let n = 1000;
    let len = 10;

    let input = (0..n)
        .flat_map(|_| repeat("a").take(len).chain(once(" ")))
        .join("");

    c.bench_function(&format!("lex identifier ({} x {})", n, len), |b| {
        b.iter(|| lexer::Lexer::new(black_box(&input)))
    });
}

fn benchmark_many_numbers(c: &mut Criterion) {
    let n = 1000;
    let len = 10;

    let value_str = format!("{:.*}", len - 2, std::f64::consts::PI);
    let input = (0..n).flat_map(|_| repeat(&value_str).take(n)).join(" ");

    c.bench_function(&format!("lex number ({} x {})", n, len), |b| {
        b.iter(|| lexer::Lexer::new(black_box(&input)))
    });
}

fn benchmark_many_terms(c: &mut Criterion) {
    let n = 1000;
    let len = 10;

    let value_str = format!("{:.*}", len - 2, std::f64::consts::PI);
    let identifier_str: String = repeat("a").take(len).collect();
    let term_str = format!("({} + {})", value_str, identifier_str);
    let input = (0..n).flat_map(|_| repeat(&term_str).take(n)).join(" * ");

    c.bench_function(&format!("lex term ({} x {})", n, term_str.len()), |b| {
        b.iter(|| lexer::Lexer::new(black_box(&input)))
    });
}

fn benchmark_random_sequence(c: &mut Criterion) {
    let n = 1000;

    fn random_token(rng: &mut rand::rngs::ThreadRng) -> String {
        match rng.gen_range(0, 100) {
            0..=29 => format!("{:.*}", rng.gen_range(0, 10), rng.gen::<f64>()),
            30..=59 => {
                let len = rng.gen_range(1, 10);
                rng.sample_iter(&Alphanumeric).take(len).collect()
            }
            60..=64 => "+".to_owned(),
            65..=69 => "-".to_owned(),
            70..=74 => "*".to_owned(),
            75..=79 => "/".to_owned(),
            80..=89 => "(".to_owned(),
            90..=99 => ")".to_owned(),
            _ => "".to_owned(),
        }
    }

    let mut rng = rand::thread_rng();
    c.bench_function(&format!("lex random sequence ({})", n), |b| {
        let input = (0..n).map(|_| random_token(&mut rng)).join(" ");
        b.iter(|| lexer::Lexer::new(black_box(&input)))
    });
}

criterion_group! {
    name = benches;
    config = Criterion::default();
    targets = benchmark_long_identifier,
              benchmark_many_identifiers,
              benchmark_many_numbers,
              benchmark_many_terms,
              benchmark_random_sequence
}
criterion_main!(benches);
