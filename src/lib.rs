use std::{borrow::Borrow, rc::Rc, time::Instant};

use lexpr::{self};

#[derive(Clone, Debug, PartialEq)]
enum ExprC<'a> {
    IfC {
        condition: Box<ExprC<'a>>,
        then: Box<ExprC<'a>>,
        els: Box<ExprC<'a>>,
    },
    NumC(f64),
    IdC(&'a str),
    AppC {
        fun: Box<ExprC<'a>>,
        args: Vec<ExprC<'a>>,
    },
    CloC {
        parameters: Vec<&'a str>,
        body: Box<ExprC<'a>>,
    },
    StringC(&'a str),
}

#[derive(Clone, Debug, PartialEq)]
enum Value<'a> {
    NumV(f64),
    CloV {
        parameters: &'a Vec<&'a str>,
        body: &'a ExprC<'a>,
        environment: Environment<'a>,
    },
    StringV(&'a str),
    BoolV(bool),
    Intrinsic(Intrinsic<'a>),
}

#[derive(Clone, Copy, Debug, PartialEq)]
enum Intrinsic<'a> {
    Binary(fn(Value<'a>, Value<'a>) -> Value<'a>),
    Unary(fn(Value<'a>) -> Value<'a>),
}

impl<'a> Value<'a> {
    fn as_bool(&self) -> Option<bool> {
        match self {
            &Value::BoolV(bool) => Some(bool),
            _ => None,
        }
    }
}

impl<'a> From<f64> for Value<'a> {
    fn from(num: f64) -> Self {
        Value::NumV(num)
    }
}

impl<'a> From<bool> for Value<'a> {
    fn from(bool: bool) -> Self {
        Value::BoolV(bool)
    }
}

#[derive(Clone, Debug, PartialEq)]
struct BindingV<'a> {
    pub from: &'a str,
    pub to: Value<'a>,
}

type Environment<'a> = Rc<Cons<BindingV<'a>>>;

#[derive(Clone, Debug, PartialEq)]
enum Cons<T> {
    Empty,
    Some{ value:T, next: Rc<Cons<T>> }
}

impl<T> Cons<T> {
    fn new() -> Rc<Self> {
        Rc::new(Cons::Empty)
    }

    fn from_vec(vec: Vec<T>) -> Rc<Self> {
        let mut iter = Self::new();
        for value in vec.into_iter() {
            iter = iter.insert(value);
        }
        iter
    }
}

trait ConsFunctionality<T> {
    fn insert(&self, value:T) -> Self;
    fn extend(&self, it: impl Iterator<Item = T>) -> Self;
}

impl<T>ConsFunctionality<T> for Rc<Cons<T>> {
    fn insert(&self, value: T) -> Self {
        Rc::new(Cons::Some { value, next: Rc::clone(self) })
    }

    fn extend(&self, it: impl Iterator<Item = T>) -> Self {
        let mut cur = self.clone();
        for value in it {
            cur = cur.insert(value);
        }
        cur
    }
}

trait LexprConsExtensions {
    fn to_shared_vec(&self) -> Vec<&lexpr::Value>;
}

impl LexprConsExtensions for lexpr::Cons {
    fn to_shared_vec(&self) -> Vec<&lexpr::Value> {
        self.list_iter().collect()
    }
}

fn parse(sexp: &lexpr::Value) -> ExprC<'_> {
    match sexp {
        lexpr::Value::Symbol(id) => ExprC::IdC(id),
        lexpr::Value::Number(num) => ExprC::NumC(num.as_f64().unwrap()),
        lexpr::Value::String(string) => ExprC::StringC(string),
        lexpr::Value::Cons(cons) => {
            let tokens = cons.to_shared_vec();
            match &tokens[..] {
                [lexpr::Value::Symbol(if_token), condition, then, els] if &**if_token == "if" => {
                    ExprC::IfC {
                        condition: Box::new(parse(condition)),
                        then: Box::new(parse(then)),
                        els: Box::new(parse(els)),
                    }
                }
                [lexpr::Value::Symbol(var), lexpr::Value::Cons(bindings_any), lexpr::Value::Symbol(in_keyword), body]
                    if &**var == "var" && &**in_keyword == "in" =>
                {
                    let bindings: Vec<_> = bindings_any
                        .to_shared_vec()
                        .iter()
                        .map(|cons_any| {
                            parse_binding(
                                &cons_any
                                    .as_cons()
                                    .expect("unexpected binding syntax")
                                    .to_shared_vec(),
                            )
                        })
                        .collect();

                    *create_binding_chain(&bindings, body)
                }
                [params @ .., lexpr::Value::Symbol(fn_def), body] if &**fn_def == "=>" => {
                    ExprC::CloC {
                        body: Box::new(parse(body)),
                        parameters: params
                            .iter()
                            .map(|symbol_any| {
                                symbol_any.as_symbol().expect("Params and name are strings")
                            })
                            .collect(),
                    }
                }
                [fun, args @ ..] => ExprC::AppC {
                    fun: Box::new(parse(fun)),
                    args: args.iter().map(|sexp| parse(sexp)).collect(),
                },
                _ => panic!("`parse` Unexpected syntax while parsing: {}", sexp),
            }
        }
        _ => panic!("`parse` Unexpected syntax while parsing: {}", sexp),
    }
}

fn create_binding_chain<'a>(
    bindings: &[(&'a str, ExprC<'a>)],
    last: &'a lexpr::Value,
) -> Box<ExprC<'a>> {
    match bindings {
        [] => Box::new(parse(last)),
        [(from, to), rst @ ..] => Box::new(ExprC::AppC {
            fun: Box::new(ExprC::CloC {
                parameters: vec![from],
                body: create_binding_chain(rst, last),
            }),
            args: vec![to.clone()],
        }),
    }
}

fn parse_binding<'a>(tokens: &Vec<&'a lexpr::Value>) -> (&'a str, ExprC<'a>) {
    match &tokens[..] {
        [lexpr::Value::Symbol(from), lexpr::Value::Symbol(equals), to] if &**equals == "=" => {
            (from, parse(to))
        }
        _ => panic!("`parse_binding` incorrect syntax for binding {:?}", tokens),
    }
}

fn environment_lookup<'a, 'b>(id: &str, env: &'b Environment<'a>) -> Value<'a> {
    match env.borrow() {
        Cons::Some { value: BindingV { from, to }, next:_ } if from == &id => {
            to.clone()
        }
        Cons::Some { value:_, next } => environment_lookup(id, next),
        Cons::Empty => panic!("JILI unbound identifier: {}", id)
    }
}

fn interp<'a, 'b>(exp: &'a ExprC<'a>, env: &'b Environment<'a>) -> Value<'a> {
    match exp {
        ExprC::NumC(num) => Value::NumV(*num),
        ExprC::StringC(string) => Value::StringV(string),
        ExprC::IdC(id) => environment_lookup(id, env),
        ExprC::IfC {
            condition,
            then,
            els,
        } => {
            if interp(condition, env)
                .as_bool()
                .expect("Conditional check must be a boolean")
            {
                interp(then, env)
            } else {
                interp(els, env)
            }
        }
        ExprC::CloC { parameters, body } => Value::CloV {
            body,
            parameters,
            environment: env.clone(),
        },
        ExprC::AppC { fun, args } => {
            let value = match &**fun {
                ExprC::IdC(name) => environment_lookup(name, env),
                fn_exp => interp(fn_exp, env),
            };
            match value.borrow() {
                Value::CloV {
                    parameters,
                    body,
                    environment: clo_env,
                } => {
                    if parameters.len() != args.len() {
                        panic!(
                            "Error calling function. Have parameters: {:?} but arguments: {:?}",
                            parameters, args
                        )
                    }
                    let next_env = clo_env.clone();

                    let extension = parameters
                    .iter()
                    .zip(args.iter().map(|arg| interp(arg, env)))
                    .map(|(from, to)| BindingV { from, to });
                    let extended_env = next_env.extend(extension);

                    interp(body, &extended_env)
                }
                Value::Intrinsic(Intrinsic::Binary(binary_op)) => match &args[..] {
                    [left, right] => binary_op(interp(left, env), interp(right, env)),
                    _ => panic!(
                        "Can not apply binary op intrinsic without exactly 2 arguments {:?}",
                        args
                    ),
                },
                Value::Intrinsic(Intrinsic::Unary(unary_op)) => match &args[..] {
                    [arg] => unary_op(interp(arg, env)),
                    _ => panic!(
                        "Can not apply unary op intrinsic without exactly 1 arguments {:?}",
                        args
                    ),
                },
                non_callable => panic!("Tried to call non callable value: {:?}", non_callable),
            }
        }
    }
}

fn serialize(value: &Value) -> String {
    match value {
        Value::Intrinsic(_) => "#<primop>".into(),
        Value::CloV {
            parameters: _,
            body: _,
            environment: _,
        } => "#<procedure>".into(),
        Value::BoolV(bool) => bool.to_string(),
        Value::NumV(num) => num.to_string(),
        Value::StringV(string) => string.to_string(),
    }
}

macro_rules! jili_binary_arith {
    ($op:tt) => {
        BindingV{
            from: stringify!($op),
            to: Value::Intrinsic(Intrinsic::Binary(|left, right| {
                match(left.borrow(),right.borrow()) {
                    (Value::NumV(left), Value::NumV(right)) => (left $op right).into(),
                    _ => panic!("Intrinsic functions require numeric arguments")
                }
        }))}
    }
}

fn jili_error<'a>(value: Value) -> Value<'a> {
    panic!("JILI user-error got: {}", serialize(&value))
}

pub fn top_interp(source: &str) -> String {
    let sexp = lexpr::from_str(source).unwrap();
    let prog = parse(&sexp);

    let base_env: Environment = Cons::from_vec(vec![
        jili_binary_arith!(+),
        jili_binary_arith!(-),
        jili_binary_arith!(*),
        jili_binary_arith!(/),
        jili_binary_arith!(<=),
        BindingV {
            from: "equal?",
            to: Value::Intrinsic(Intrinsic::Binary(|a, b| {
                Value::BoolV(a == b)
            })),
        },
        BindingV {
            from: "error",
            to: Value::Intrinsic(Intrinsic::Unary(jili_error)),
        },
        BindingV {
            from: "true",
            to: Value::BoolV(true),
        },
        BindingV {
            from: "false",
            to: Value::BoolV(false),
        },
    ]);

    let now = Instant::now();
    let result = interp(&prog, &base_env);
    println!("program execution took: {}", now.elapsed().as_millis());

    serialize(&result)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn top_interp_tests() {
        assert_eq!(top_interp("5"), "5", "Most basic Program");
        assert_eq!(
            top_interp("((x => (+ x 6)) 5)"),
            "11",
            "Binary operation program"
        );
        assert_eq!(top_interp("((=> 7))"), "7", "No argument lambda");
        assert_eq!(
            top_interp("(((x => (x => (+ 3 x)))5)7)"),
            "10",
            "Shadowing works"
        );
        assert_eq!(top_interp("(((=> +)) 3 4)"), "7", "return intrinsic");
        assert_eq!(top_interp("(var ((x = 5)) in x)"), "5", "Basic Binding");
        assert_eq!(
            top_interp(
                "(var (
                    (hi = 5)
                    (bye = (+ 2 hi))
                    (f = (=> (+ hi bye)))
                    ) in
                    (f))"
            ),
            "12",
            "More complex binding"
        );
        assert_eq!(
            top_interp(
                "(var (
                    (fib = (x fib => (if (<= x 1) x (+ (fib (- x 1) fib) (fib (- x 2) fib)))))
                ) in (fib 17 fib))"
            ),
            "1597",
            "Fibonacci sequence"
        );
        assert_eq!(top_interp("+"), "#<primop>", "Builtin");
        assert_eq!(top_interp("(a b => (+ a b))"), "#<procedure>", "defined");
        assert_eq!(top_interp("true"), "true", "boolean");
        assert_eq!(top_interp(r#""Hello World""#), "Hello World", "string");
        assert_eq!(
            top_interp(r#"((=> "Hello World"))"#),
            "Hello World",
            "fn returns string"
        );
        assert_eq!(
            top_interp(r#"(equal? "Hello World" "Hello World")"#),
            "true",
            "strings equal"
        );
        assert_eq!(
            top_interp(
                "(var (
                    (fact = (x fact => (if (equal? x 0) 1 (* x (fact (- x 1) fact)))))
                ) in (fact 12 fact))"
            ),
            "479001600",
            "Factorial of 12"
        )
    }

    #[test]
    #[should_panic(expected = "JILI user-error got: Hello")]
    fn error_causes_panic() {
        top_interp(r#"(error "Hello")"#);
    }
}
