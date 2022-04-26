use lexpr::{self};

#[derive(Clone, Debug, PartialEq)]
enum ExprC {
    Leq0C {
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
    CloC(CloC),
}

#[derive(Clone, Debug, PartialEq)]
struct CloC {
    pub name: Option<String>,
    pub parameters: Vec<String>,
    pub body: Box<ExprC>,
}

#[derive(Clone, Debug, PartialEq)]
enum Value {
    NumV(f64),
    CloV(CloV),
    BinaryOpIntrinsic(fn(Value, Value) -> Value),
}

impl Value {
    fn as_num(&self) -> Option<f64> {
        match self {
            &Value::NumV(num) => Some(num),
            _ => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq)]
struct CloV {
    pub parameters: Vec<String>,
    pub body: ExprC,
    pub environment: Environment,
}

#[derive(Clone, Debug, PartialEq)]
struct BindingV {
    pub from: String,
    pub to: Value,
}

type Environment = Vec<BindingV>;

fn main() {
    let source = "(
        (def (main) (fib 17))
        (def (fib x) (leq0? (- x 1) x (+ (fib (- x 1)) (fib (- x 2)))))
    )";
    let result = top_interp(source);
    println!("Result: {:?}", result);
}

fn parse_prog(sexp: &lexpr::Value) -> Vec<CloC> {
    match sexp {
        lexpr::Value::Nil | lexpr::Value::Null => {
            vec![]
        }
        lexpr::Value::Cons(cons) => {
            let (fun, rest) = cons.as_pair();
            let mut parsed_closures = vec![parse_cloc(fun)];
            parsed_closures.append(&mut parse_prog(rest));
            parsed_closures
        }
        _ => panic!("`parse_prog` Unexpected syntax while parsing: {}", sexp),
    }
}

fn parse_cloc(sexp: &lexpr::Value) -> CloC {
    let cons = sexp.as_cons().expect(&format!(
        "`parse_cloC` Unexpected syntax while parsing: {}",
        sexp
    ));
    match &cons.to_vec().0[..] {
        [lexpr::Value::Symbol(def), lexpr::Value::Cons(name_and_params_cons), body]
            if &**def == "def" =>
        {
            let name_and_params: Vec<String> = name_and_params_cons
                .iter()
                .map(|cons| {
                    cons.car()
                        .as_symbol()
                        .expect("Params and name are strings")
                        .to_owned()
                })
                .collect();
            CloC {
                name: Some(name_and_params[0].clone()),
                body: Box::new(parse(body)),
                parameters: name_and_params[1..].to_owned(),
            }
        }
        _ => panic!("`parse_cloV` Unexpected syntax while parsing: {}", sexp),
    }
}

fn parse(sexp: &lexpr::Value) -> ExprC {
    match sexp {
        lexpr::Value::Symbol(id) => ExprC::IdC(id.to_string()),
        lexpr::Value::Number(num) => ExprC::NumC(num.as_f64().unwrap()),
        lexpr::Value::Cons(cons) => {
            let tokens = cons.to_vec().0;
            match &tokens[..] {
                [lexpr::Value::Symbol(leq0), condition, then, els] if &**leq0 == "leq0?" => {
                    println!("leq0?");
                    ExprC::Leq0C {
                        condition: Box::new(parse(condition)),
                        then: Box::new(parse(then)),
                        els: Box::new(parse(els)),
                    }
                }
                [bindings_any @ .., last]
                    if bindings_any.len() > 0
                        && bindings_any.iter().all(|binding| {
                            binding
                                .as_cons()
                                .map(|cons| cons.car().as_symbol().map(|str| str == "let"))
                                .flatten()
                                .unwrap_or(false)
                        }) =>
                {
                    let bindings: Vec<_> = bindings_any
                        .iter()
                        .map(|cons_any| {
                            parse_binding(
                                cons_any
                                    .as_cons()
                                    .expect("unexpected binding syntax")
                                    .to_vec()
                                    .0,
                            )
                        })
                        .collect();

                    fn create_chain(
                        bindings: Vec<(String, ExprC)>,
                        last: &lexpr::Value,
                    ) -> Box<ExprC> {
                        match &bindings[..] {
                            [] => Box::new(parse(last)),
                            [(from, to), rst @ ..] => Box::new(ExprC::AppC {
                                fun: Box::new(ExprC::CloC(CloC {
                                    name: None,
                                    parameters: vec![from.clone()],
                                    body: create_chain(rst.to_vec(), last),
                                })),
                                args: vec![to.clone()],
                            }),
                        }
                    }
                    *create_chain(bindings, last)
                }
                [lexpr::Value::Symbol(def), lexpr::Value::Cons(params), body]
                    if &**def == "lambda" =>
                {
                    ExprC::CloC(CloC {
                        name: None,
                        body: Box::new(parse(body)),
                        parameters: params
                            .iter()
                            .map(|cons| {
                                cons.car()
                                    .as_symbol()
                                    .expect("Params and name are strings")
                                    .to_owned()
                            })
                            .collect(),
                    })
                }
                [lexpr::Value::Symbol(def), lexpr::Value::Null, body] if &**def == "lambda" => {
                    ExprC::CloC(CloC {
                        name: None,
                        body: Box::new(parse(body)),
                        parameters: vec![],
                    })
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

fn parse_binding(tokens: Vec<lexpr::Value>) -> (String, ExprC) {
    match &tokens[..] {
        [lexpr::Value::Symbol(let_binding), lexpr::Value::Symbol(from), to]
            if &**let_binding == "let" =>
        {
            (from.to_string(), parse(to))
        }
        _ => panic!("`parse_binding` incorrect syntax for binding {:?}", tokens),
    }
}

macro_rules! jili_arith {
    ($op:tt) => {
        BindingV{
            from: stringify!($op).to_string()
        ,to: Value::BinaryOpIntrinsic(|left:Value, right: Value| {
            match(left,right) {
                (Value::NumV(left), Value::NumV(right)) => Value::NumV(left $op right),
                _ => panic!("Intrinsic functions require numeric arguments")
            }
        })}
    };
}

fn interp_fns(fns: Vec<CloC>) -> Value {
    let main = fns
        .iter()
        .find(|clov| clov.name == Some("main".to_string()))
        .expect("No main function found")
        .clone();
    let mut base_env: Environment = vec![
        jili_arith!(+),
        jili_arith!(-),
        jili_arith!(*),
        jili_arith!(/),
    ];

    base_env.extend(fns.into_iter().map(|closure| BindingV {
        from: closure.name.unwrap(),
        to: Value::CloV(CloV {
            body: *closure.body,
            parameters: closure.parameters,
            environment: vec![],
        }),
    }));

    interp(&main.body, &base_env, &base_env)
}

fn environment_lookup(id: &str, env: &Environment) -> Value {
    for binding in env.iter().rev() {
        if binding.from == id {
            return binding.to.clone();
        }
    }
    panic!("JILI unbound identifier: {}", id);
}

fn interp(exp: &ExprC, env: &Environment, base_env: &Environment) -> Value {
    match exp {
        ExprC::NumC(num) => Value::NumV(*num),
        ExprC::IdC(id) => environment_lookup(&id, &env),
        ExprC::Leq0C {
            condition,
            then,
            els,
        } => {
            if interp(condition, env, base_env)
                .as_num()
                .expect("Conditional must result in a number")
                <= 0.
            {
                interp(then, env, base_env)
            } else {
                interp(els, env, base_env)
            }
        }
        ExprC::CloC(CloC {
            name: _,
            parameters,
            body,
        }) => Value::CloV(CloV {
            body: *body.clone(),
            parameters: parameters.clone(),
            environment: env.clone(),
        }),
        ExprC::AppC { fun, args } => {
            let value = match &**fun {
                ExprC::IdC(name) => environment_lookup(&name, &env),
                fn_exp => interp(&fn_exp, env, base_env),
            };
            match value {
                Value::NumV(_) => panic!("Can not call a number"),
                Value::CloV(CloV {
                    parameters,
                    body,
                    environment,
                }) => {
                    if parameters.len() != args.len() {
                        panic!(
                            "Error calling function. Have parameters: {:?} but arguments: {:?}",
                            parameters, args
                        )
                    }
                    let mut next_env = environment.clone();
                    next_env.append(&mut env.clone());
                    next_env.extend(
                        parameters
                            .into_iter()
                            .zip(args.into_iter().map(|arg| interp(arg, env, base_env)))
                            .map(|(from, to)| BindingV { from, to }),
                    );

                    interp(&body, &next_env, base_env)
                }
                Value::BinaryOpIntrinsic(operation) => match &args[..] {
                    [left, right] => {
                        operation(interp(left, env, base_env), interp(right, env, base_env))
                    }
                    _ => panic!(
                        "Can not apply binary op intrinsic without exactly 2 arguments {:?}",
                        args
                    ),
                },
            }
        }
    }
}

fn top_interp(source: &str) -> Value {
    let prog = parse_prog(&lexpr::from_str(&source).unwrap());
    println!("Prog {:?}", prog);
    interp_fns(prog)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn top_interp_tests() {
        assert_eq!(
            top_interp("((def (main) 5))"),
            Value::NumV(5.),
            "Most basic Program"
        );
        assert_eq!(
            top_interp("((def (main) (+ 5 6)))"),
            Value::NumV(11.),
            "Binary operation program"
        );
        assert_eq!(
            top_interp(
                "(
                (def (f y) (lambda (x) (+ x y)))
                (def (main) ((f 2) 5))
            )"
            ),
            Value::NumV(7.),
            "Simple closure"
        );
        assert_eq!(
            top_interp(
                "(
                (def (f y) (lambda () (+ 5 y)))
                (def (main) ((f 2))))"
            ),
            Value::NumV(7.),
            "No argument lambda"
        );
        assert_eq!(
            top_interp(
                "(
                (def (f x) (lambda (x) (+ x 5)))
                (def (main) ((f 2) 5))
            )"
            ),
            Value::NumV(10.),
            "Shadowing works"
        );
        assert_eq!(
            top_interp(
                "(
                (def (my-add) +)
                (def (main) ((my-add) 2 5))
            )"
            ),
            Value::NumV(7.),
            "return intrinsic"
        );
        assert_eq!(
            top_interp(
                "(
                (def (main) ((let hi 5) hi))
            )"
            ),
            Value::NumV(5.),
            "Basic Binding"
        );
        assert_eq!(
            top_interp(
                "((def (main) (
                    (let hi 5)
                    (let bye (+ 2 hi))
                    (let f (lambda () (+ hi bye)))
                    (f))))"
            ),
            Value::NumV(12.),
            "More complex binding"
        );
        assert_eq!(
            top_interp(
                "(
            (def (main) (fib 17))
            (def (fib x) (leq0? (- x 1) x (+ (fib (- x 1)) (fib (- x 2)))))
        )"
            ),
            Value::NumV(1597.),
            "Fibonacci sequence"
        );
    }
}
