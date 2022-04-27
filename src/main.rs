use lexpr::{self};

#[derive(Clone, Debug, PartialEq)]
enum ExprC {
    IfC {
        condition: Box<ExprC>,
        then: Box<ExprC>,
        els: Box<ExprC>,
    },
    NumC(f64),
    IdC(String),
    AppC {
        fun: Box<ExprC>,
        args: Vec<ExprC>,
    },
    CloC {
        parameters: Vec<String>,
        body: Box<ExprC>,
    },
    StringC(String),
}

#[derive(Clone, Debug, PartialEq)]
enum Value {
    NumV(f64),
    CloV {
        parameters: Vec<String>,
        body: ExprC,
        environment: Environment,
    },
    StringV(String),
    BoolV(bool),
    Intrinsic(Intrinsic),
}

#[derive(Clone, Debug, PartialEq)]
enum Intrinsic {
    Binary(fn(Value, Value) -> Value),
    Unary(fn(Value) -> Value),
}

impl Value {
    fn as_bool(&self) -> Option<bool> {
        match self {
            &Value::BoolV(bool) => Some(bool),
            _ => None,
        }
    }
}

impl From<f64> for Value {
    fn from(num: f64) -> Self {
        Value::NumV(num)
    }
}

impl From<bool> for Value {
    fn from(bool: bool) -> Self {
        Value::BoolV(bool)
    }
}

#[derive(Clone, Debug, PartialEq)]
struct BindingV {
    pub from: String,
    pub to: Value,
}

type Environment = Vec<BindingV>;

fn main() {
    let source = "((var (
        (fib = (x => (if (<= x 1) x (+ (fib (- x 1)) (fib (- x 2))))))
    ) in (fib 17)))";
    let result = top_interp(source);
    println!("Fibonacci of 17 is: {:?}", result);
}

fn parse(sexp: &lexpr::Value) -> ExprC {
    match sexp {
        lexpr::Value::Symbol(id) => ExprC::IdC(id.to_string()),
        lexpr::Value::Number(num) => ExprC::NumC(num.as_f64().unwrap()),
        lexpr::Value::String(string) => ExprC::StringC(string.to_string()),
        lexpr::Value::Cons(cons) => {
            let tokens = cons.to_vec().0;
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
                        .to_vec()
                        .0
                        .iter()
                        .map(|cons_any| {
                            parse_binding(
                                &cons_any
                                    .as_cons()
                                    .expect("unexpected binding syntax")
                                    .to_vec()
                                    .0,
                            )
                        })
                        .collect();

                    fn create_chain(
                        bindings: &[(String, ExprC)],
                        last: &lexpr::Value,
                    ) -> Box<ExprC> {
                        match &bindings[..] {
                            [] => Box::new(parse(last)),
                            [(from, to), rst @ ..] => Box::new(ExprC::AppC {
                                fun: Box::new(ExprC::CloC {
                                    parameters: vec![from.clone()],
                                    body: create_chain(rst, last),
                                }),
                                args: vec![to.clone()],
                            }),
                        }
                    }
                    *create_chain(&bindings, body)
                }
                [params @ .., lexpr::Value::Symbol(fn_def), body] if &**fn_def == "=>" => {
                    ExprC::CloC {
                        body: Box::new(parse(body)),
                        parameters: params
                            .iter()
                            .map(|symbol_any| {
                                symbol_any
                                    .as_symbol()
                                    .expect("Params and name are strings")
                                    .to_owned()
                            })
                            .collect(),
                    }
                }
                [fun, args @ ..] => ExprC::AppC {
                    fun: Box::new(parse(fun)),
                    args: args.into_iter().map(|sexp| parse(sexp)).collect(),
                },
                _ => panic!("`parse` Unexpected syntax while parsing: {}", sexp),
            }
        }
        _ => panic!("`parse` Unexpected syntax while parsing: {}", sexp),
    }
}

fn parse_binding(tokens: &Vec<lexpr::Value>) -> (String, ExprC) {
    match &tokens[..] {
        [lexpr::Value::Symbol(from), lexpr::Value::Symbol(equals), to] if &**equals == "=" => {
            (from.to_string(), parse(to))
        }
        _ => panic!("`parse_binding` incorrect syntax for binding {:?}", tokens),
    }
}

fn environment_lookup(id: &str, env: &Environment) -> Value {
    for binding in env.iter().rev() {
        if binding.from == id {
            return binding.to.clone();
        }
    }
    panic!("JILI unbound identifier: {}", id);
}

fn interp(exp: &ExprC, env: &Environment) -> Value {
    match exp {
        ExprC::NumC(num) => Value::NumV(*num),
        ExprC::StringC(string) => Value::StringV(string.clone()),
        ExprC::IdC(id) => environment_lookup(&id, &env),
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
            body: *body.clone(),
            parameters: parameters.clone(),
            environment: env.clone(),
        },
        ExprC::AppC { fun, args } => {
            let value = match &**fun {
                ExprC::IdC(name) => environment_lookup(&name, &env),
                fn_exp => interp(&fn_exp, env),
            };
            match value {
                Value::CloV {
                    parameters,
                    body,
                    environment:clo_env,
                } => {
                    if parameters.len() != args.len() {
                        panic!(
                            "Error calling function. Have parameters: {:?} but arguments: {:?}",
                            parameters, args
                        )
                    }
                    let mut next_env = clo_env.clone();
                    next_env.extend(
                        parameters
                            .into_iter()
                            .zip(args.into_iter().map(|arg| interp(arg, env)))
                            .map(|(from, to)| BindingV { from, to }),
                    );

                    interp(&body, &next_env)
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

fn serialize(value: Value) -> String {
    match value {
        Value::Intrinsic(_) => "#<primop>".into(),
        Value::CloV {
            parameters: _,
            body: _,
            environment: _,
        } => "#<procedure>".into(),
        Value::BoolV(bool) => bool.to_string(),
        Value::NumV(num) => num.to_string(),
        Value::StringV(string) => string,
    }
}

macro_rules! jili_binary_arith {
    ($op:tt) => {
        BindingV{
            from: stringify!($op).to_string(),
            to: Value::Intrinsic(Intrinsic::Binary(|left:Value, right: Value| {
                match(left,right) {
                    (Value::NumV(left), Value::NumV(right)) => (left $op right).into(),
                    _ => panic!("Intrinsic functions require numeric arguments")
                }
        }))}
    }
}

fn jili_error(value: Value) -> Value {
    panic!("JILI user-error got: {}", serialize(value))
}

fn top_interp(source: &str) -> String {
    let prog = parse(&lexpr::from_str(&source).unwrap());
    let base_env: Environment = vec![
        jili_binary_arith!(+),
        jili_binary_arith!(-),
        jili_binary_arith!(*),
        jili_binary_arith!(/),
        jili_binary_arith!(<=),
        BindingV {
            from: "equals?".into(),
            to: Value::Intrinsic(Intrinsic::Binary(|a, b| (a == b).into())),
        },
        BindingV {
            from: "error".into(),
            to: Value::Intrinsic(Intrinsic::Unary(jili_error)),
        },
        BindingV {
            from: "true".into(),
            to: Value::BoolV(true),
        },
        BindingV {
            from: "false".into(),
            to: Value::BoolV(false),
        },
    ];

    let result = interp(&prog, &base_env);
    serialize(result)
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
            top_interp(r#"(equals? "Hello World" "Hello World")"#),
            "true",
            "strings equal"
        );
    }

    #[test]
    #[should_panic(expected = "JILI user-error got: Hello")]
    fn error_causes_panic() {
        top_interp(r#"(error "Hello")"#);
    }
}
