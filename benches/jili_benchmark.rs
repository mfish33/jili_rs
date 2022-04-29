use criterion::{black_box, criterion_group, criterion_main, Criterion};
use jili_rs::top_interp;

fn jili_fib(x: i32) -> String {
    let source = format!(
        "(var (
        (fib = (x fib => (if (<= x 1) x (+ (fib (- x 1) fib) (fib (- x 2) fib)))))
    ) in (fib {} fib))",
        x
    );
    top_interp(&source)
}

fn jili_add_higher_order(x: i32) -> String {
    let source = format!(
        "(var (
        (one = (fn arg => (fn arg)))
        (two = (fn arg => (fn (fn arg))))
        (add = (num-like1 num-like2 => (fn arg => (num-like2 fn (num-like1 fn arg)))))
        (plus-one = (x => (+ x 1)))
    ) in ((add (add one two) two) plus-one {}))",
        x
    );
    top_interp(&source)
}

fn jili_factorial(x: i32) -> String {
    let source = format!(
        "(var (
            (fact = (x fact => (if (equal? x 0) 1 (* x (fact (- x 1) fact)))))
        ) in (fact {} fact))",
        x
    );
    top_interp(&source)
}

fn criterion_benchmark(c: &mut Criterion) {
    c.bench_function("jili_fib 25", |b| b.iter(|| jili_fib(black_box(25))));
    c.bench_function("jili_add_higher_order 10", |b| {
        b.iter(|| jili_add_higher_order(black_box(10)))
    });
    c.bench_function("jili_factorial 12", |b| {
        b.iter(|| jili_factorial(black_box(12)))
    });
}

criterion_group!(benches, criterion_benchmark);
criterion_main!(benches);
