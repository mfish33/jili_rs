use std::{borrow::Borrow, rc::Rc, time::Instant};

use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::{char, multispace0},
    multi::{many0, many1},
    number::complete::double,
    sequence::{delimited, tuple},
    IResult,
};

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
    Some { value: T, next: Rc<Cons<T>> },
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
    fn insert(&self, value: T) -> Self;
    fn extend(&self, it: impl Iterator<Item = T>) -> Self;
}

impl<T> ConsFunctionality<T> for Rc<Cons<T>> {
    fn insert(&self, value: T) -> Self {
        Rc::new(Cons::Some {
            value,
            next: Rc::clone(self),
        })
    }

    fn extend(&self, it: impl Iterator<Item = T>) -> Self {
        let mut cur = self.clone();
        for value in it {
            cur = cur.insert(value);
        }
        cur
    }
}

fn parse_exprc(input: &str) -> IResult<&str, ExprC> {
    delimited(
        multispace0,
        alt((
            parse_cloc,
            parse_ifc,
            parse_var,
            parse_appc,
            parse_stringc,
            parse_numc,
            parse_idc,
        )),
        multispace0,
    )(input)
}

fn parse_numc(input: &str) -> IResult<&str, ExprC> {
    let (rest, result) = double(input)?;
    Ok((rest, ExprC::NumC(result)))
}

fn parse_idc(input: &str) -> IResult<&str, ExprC> {
    parse_legal_id(input).map(|(rest, res)| (rest, ExprC::IdC(res)))
}

fn parse_stringc(input: &str) -> IResult<&str, ExprC> {
    delimited(char('"'), take_while(|char| char != '"'), char('"'))(input)
        .map(|(rest, res)| (rest, ExprC::StringC(res)))
}

fn parse_legal_id(input: &str) -> IResult<&str, &str> {
    let result =
        take_while1(|char| !char::is_whitespace(char) && char != '(' && char != ')')(input)?;
    if result.1 == "var" || result.1 == "=>" || result.1 == "if" {
        Err(nom::Err::Error(nom::error::Error::new(
            input,
            nom::error::ErrorKind::Verify,
        )))
    } else {
        Ok(result)
    }
}

fn parse_appc(input: &str) -> IResult<&str, ExprC> {
    let (rest, mut result) = delimited(tag("("), many1(parse_exprc), tag(")"))(input)?;
    let fun = Box::new(result.remove(0));
    Ok((rest, ExprC::AppC { fun, args: result }))
}

fn parse_ifc(input: &str) -> IResult<&str, ExprC> {
    let (rest, (_, _, _, condition, then, els, _, _)) = tuple((
        tag("("),
        multispace0,
        tag("if"),
        parse_exprc,
        parse_exprc,
        parse_exprc,
        multispace0,
        tag(")"),
    ))(input)?;
    Ok((
        rest,
        ExprC::IfC {
            condition: Box::new(condition),
            then: Box::new(then),
            els: Box::new(els),
        },
    ))
}

fn parse_var(input: &str) -> IResult<&str, ExprC> {
    let (rest, (_, _, _, bindings, _, _, _, body, _)) = tuple((
        char('('),
        delimited(multispace0, tag("var"), multispace0),
        char('('),
        many1(delimited(multispace0, parse_binding, multispace0)),
        char(')'),
        multispace0,
        tag("in"),
        delimited(multispace0, parse_exprc, multispace0),
        char(')'),
    ))(input)?;

    let generated_exprc = ExprC::AppC {
        fun: Box::new(ExprC::CloC {
            parameters: bindings.iter().map(|(id, _)| *id).collect(),
            body: Box::new(body),
        }),
        args: bindings.iter().map(|(_, value)| value.clone()).collect(),
    };
    Ok((rest, generated_exprc))
}

fn parse_binding(input: &str) -> IResult<&str, (&str, ExprC)> {
    let (rest, (_, id, _, value, _)) = tuple((
        char('('),
        delimited(multispace0, parse_legal_id, multispace0),
        char('='),
        delimited(multispace0, parse_exprc, multispace0),
        char(')'),
    ))(input)?;
    Ok((rest, (id, value)))
}

fn parse_cloc(input: &str) -> IResult<&str, ExprC> {
    let (rest, (_, parameters, _, _, _, body, _)) = tuple((
        tag("("),
        many0(delimited(multispace0, parse_legal_id, multispace0)),
        multispace0,
        tag("=>"),
        multispace0,
        parse_exprc,
        tag(")"),
    ))(input)?;
    Ok((
        rest,
        ExprC::CloC {
            parameters,
            body: Box::new(body),
        },
    ))
}

fn environment_lookup<'a, 'b>(id: &str, env: &'b Environment<'a>) -> Value<'a> {
    match env.borrow() {
        Cons::Some {
            value: BindingV { from, to },
            next: _,
        } if from == &id => to.clone(),
        Cons::Some { value: _, next } => environment_lookup(id, next),
        Cons::Empty => panic!("JILI unbound identifier: {}", id),
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
    // let sexp = lexpr::from_str(source).unwrap();
    let (_, prog) = parse_exprc(source).unwrap();

    let base_env: Environment = Cons::from_vec(vec![
        jili_binary_arith!(+),
        jili_binary_arith!(-),
        jili_binary_arith!(*),
        jili_binary_arith!(/),
        jili_binary_arith!(<=),
        BindingV {
            from: "equal?",
            to: Value::Intrinsic(Intrinsic::Binary(|a, b| Value::BoolV(a == b))),
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
        assert_eq!(top_interp("(( => 7))"), "7", "No argument lambda");
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

    #[test]
    fn parse_tests() {
        assert_eq!(
            parse_cloc("(x => 5)").unwrap().1,
            ExprC::CloC {
                parameters: vec!["x"],
                body: Box::new(ExprC::NumC(5.))
            }
        );
        assert_eq!(
            parse_cloc("(x y => 5)").unwrap().1,
            ExprC::CloC {
                parameters: vec!["x", "y"],
                body: Box::new(ExprC::NumC(5.))
            }
        );
        assert_eq!(
            parse_cloc("(x y => (+ x y))").unwrap().1,
            ExprC::CloC {
                parameters: vec!["x", "y"],
                body: Box::new(ExprC::AppC {
                    fun: Box::new(ExprC::IdC("+")),
                    args: vec![ExprC::IdC("x"), ExprC::IdC("y")]
                })
            }
        );
        assert_eq!(
            parse_appc("(+ 5 10)").unwrap().1,
            ExprC::AppC {
                fun: Box::new(ExprC::IdC("+")),
                args: vec![ExprC::NumC(5.), ExprC::NumC(10.)]
            }
        );
        assert!(parse_appc("((x => (+ x 10)) 1)").is_ok());
    }
}
