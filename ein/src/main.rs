use std::collections::HashMap;
use std::ops::Mul;

use nom::{
    IResult,
    branch::alt,
    bytes::complete::tag,
    character::complete::{
        u64,
        newline,
        space0,
        alpha1,
    },
    sequence::{
        separated_pair,
        tuple,
        delimited,
    },
    multi::{
        separated_list0,
        separated_list1,
    }
};

enum AxisDescription {
    Name(String),
    Size(usize),
    Subaxis(Vec<AxisDescription>),
    Concatenation(Vec<AxisDescription>),
    Target(Box<AxisDescription>)
}

struct Ein {
    before: Vec<AxisDescription>,
    after: Vec<AxisDescription>
}

enum Expression {
    Ones(Vec<usize>),
    Rearrange(Ein, Box<Expression>),
    Reduce(Ein, Box<Function>),
    Map(Ein, Box<Function>),
    Reference(String)
}

struct Function {
    assignments: Vec<(String, Expression)>,
    value: Expression,
}

fn parse_targeted_description(input: &str) -> IResult<&str, AxisDescription> {
    let (rest, subdescription) = delimited(
        tag("["),
        parse_axis_description,
        tag("]"),
    )(input)?;
    Ok((rest, AxisDescription::Target(Box::new(subdescription))))
}

fn parse_concat_axis(input: &str) -> IResult<&str, AxisDescription> {
    let (rest, descriptions) = delimited(
        tag("["),
        separated_list1(
            tuple((space0, tag("+"), space0)),
            parse_axis_description,
        ),
        tag("]"),
    )(input)?;
    Ok((rest, AxisDescription::Concatenation(descriptions)))
}

fn parse_anon_axis(input: &str) -> IResult<&str, AxisDescription> {
    let (rest, size) = u64(input)?;
    Ok((rest, AxisDescription::Size(size as usize)))
}

fn parse_axis_name(input: &str) -> IResult<&str, AxisDescription> {
    let (rest, name) = alpha1(input)?;
    Ok((rest, AxisDescription::Name(name.to_string())))
}

fn parse_axis_description(input: &str) -> IResult<&str, AxisDescription> {
    alt((
        parse_anon_axis,
        parse_axis_description,
        parse_concat_axis,
        parse_axis_name,
    ))(input)
}

fn parse_axis_list(input: &str) -> IResult<&str, Vec<AxisDescription>> {
    let (rest, axis_list) = separated_list0(
        tuple((space0, tag(","), space0)),
        parse_axis_description,
    )(input)?;
    Ok((rest, axis_list))
}

fn parse_ein(input: &str) -> IResult<&str, Ein> {
    let (rest, (before, after)) = separated_pair(
        parse_axis_list,
        tuple((space0, tag("->"), space0)),
        parse_axis_list,
    )(input)?;
    Ok((rest, Ein {
        before,
        after,
    }))
}

fn parse_reference(input: &str) -> IResult<&str, Expression> {
    let (rest, name) = alpha1(input)?;
    Ok((rest, Expression::Reference(name.to_string())))
}

fn parse_map(input: &str) -> IResult<&str, Expression> {
    let (rest, (ein, func)) = separated_pair(
        parse_ein,
        tag("/"),
        parse_function
    )(input)?;
    Ok((rest, Expression::Map(ein, Box::new(func))))
}

fn parse_reduce(input: &str) -> IResult<&str, Expression> {
    let (rest, (ein, func)) = separated_pair(
        parse_ein,
        tag("/"),
        parse_function
    )(input)?;
    Ok((rest, Expression::Reduce(ein, Box::new(func))))
}

fn parse_rearrange(input: &str) -> IResult<&str, Expression> {
    let (rest, (ein, expr)) = separated_pair(
        parse_ein,
        tag("#"),
        parse_expression
    )(input)?;
    Ok((rest, Expression::Rearrange(ein, Box::new(expr))))
}

fn parse_constant(input: &str) -> IResult<&str, Expression > {
    let (rest, parse_result) = delimited(
        tag("["),
        separated_list0(
            tuple((space0, tag(","), space0)),
            u64,
        ),
        tag("]"),
    )(input)?;
    Ok((rest, Expression::Ones(parse_result.iter().map(|n| *n as usize).collect())))
}

fn parse_expression(input: &str) -> IResult<&str, Expression> {
    alt((
        parse_constant,
        parse_rearrange,
        parse_reduce,
        parse_map,
    ))(input)
}

fn parse_assignment(input: &str) -> IResult<&str, (String, Expression)> {
    let (rest, (lhs, rhs)) = separated_pair(
                alpha1,
                tuple((space0, tag("="), space0)),
                parse_expression,
            )(input)?;
    Ok((rest, (lhs.to_string(), rhs)))

}

fn parse_function(input: &str) -> IResult<&str, Function> {
    let (rest, (assignments, retval)) = separated_pair(
        separated_list0(
            newline,
            parse_assignment,
        ),
        newline,
        parse_expression
    )(input)?;
    Ok((rest, Function {
        assignments,
        value: retval,
    }))

}

#[derive(Clone)]
struct Tensor {
    shape: Vec<usize>,
    values: Vec<f32>,
}

#[derive(Clone)]
struct Environment {
    values: HashMap<String, Tensor>,
}

impl Environment {
    fn new_empty() -> Self {
        Environment {
            values: HashMap::new()
        }
    }
}

fn recursive_descent_interpret(program: Function) -> Tensor {
    call(program, Environment::new_empty())
}

fn call(func: Function, env: Environment) -> Tensor {

}

fn shape_count(shape: Vec<usize>) -> usize {
    shape.iter().fold(1, Mul::mul)
}

fn repeat(count: usize, value: f32) -> Vec<f32> {
    let mut v = Vec::new();
    v.resize(count, value);
    v
}

fn rearrange(ein: Ein, tensor: Tensor) -> Tensor {
    // smt solver for shapes ???
    let common_axis = vec![];
    let before_names = ein.before.iter().filter_map(|axis_desc| if let AxisDescription::Name(name) = axis_desc {Some(name)} else {None} );
    let after_names = ein.after.iter().filter_map(|axis_desc| if let AxisDescription::Name(name) = axis_desc {Some(name)} else {None} );
    for axis in ein.before.iter() {
        match axis {
            AxisDescription::Name() => todo!(),
            AxisDescription::Size(_) => todo!(),
            AxisDescription::Subaxis(_) => todo!(),
            AxisDescription::Concatenation(_) => todo!(),
            AxisDescription::Target(_) => todo!(),
        }
    }
}

fn eval_expr(expr: Expression, env: Environment) -> Tensor {
    match expr {
        Expression::Ones(shape) => {
            Tensor {
                shape,
                values: repeat(shape_count(shape), 1.0),
            }
        },
        Expression::Rearrange(ein, subexpr) => {
            let subresult = eval_expr(*subexpr, env.clone());

        },
        Expression::Reduce(ein, func) => todo!(),
        Expression::Map(_, _) => todo!(),
        Expression::Reference(name) => todo!(),
    }
}

fn main() {
    println!("Hello, world!");
}
