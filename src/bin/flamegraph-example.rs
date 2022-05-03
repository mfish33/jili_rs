use jili_rs::top_interp;

fn main() {
    let x = 25;
    let source = format!(
        "(var (
        (fib = (x fib => (if (<= x 1) x (+ (fib (- x 1) fib) (fib (- x 2) fib)))))
    ) in (fib {} fib))",
        x
    );
    top_interp(&source);
}