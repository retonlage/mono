use std::io;
use std::collections::HashMap;
use std::ops::Mul;
use utils::{divrem, IteratorExt};

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

#[derive(Clone)]
struct SizedAxis {
    desc: AxisDescription,
    size: Option<usize>,
}

#[derive(Clone)]
enum AxisDescription {
    Wildcard,
    Named(String),
    Target(Box<SizedAxis>),
    Composition(Vec<SizedAxis>),
    Concatenation(Vec<SizedAxis>)
}

#[derive(Clone)]
struct Ein {
    before: Vec<SizedAxis>,
    after: Vec<SizedAxis>
}

impl Ein {
    fn flatten_compositions(&self) -> Ein {
        fn flatten_axis_descriptions(descs: &Vec<SizedAxis>) -> Vec<SizedAxis> {
            descs.iter().map(|desc| match &desc.desc {
                AxisDescription::Wildcard => vec![SizedAxis {
                    desc: AxisDescription::Wildcard,
                    size: desc.size,
                }],
                AxisDescription::Named(name) => vec![SizedAxis {
                    desc: AxisDescription::Named(name.clone()),
                    size: desc.size,
                }],
                AxisDescription::Target(sub) => vec![SizedAxis {
                    desc: AxisDescription::Target(Box::new(*sub.clone())),
                    size: desc.size,
                }],
                AxisDescription::Composition(subaxes) => {
                    flatten_axis_descriptions(&subaxes)
                },
                AxisDescription::Concatenation(_) => todo!(),
            }).flatten().collect()
        }

        Ein {
            before: flatten_axis_descriptions(&self.before),
            after: flatten_axis_descriptions(&self.after)
        }
    }
}

enum Expression {
    Ones(Vec<usize>),
    Rearrange(Ein, Box<Expression>),
    Reduce(Ein, Box<Function>),
    Map(Ein, Box<Function>),
    Index(Ein, Box<Expression>, Box<Expression>),
    Let(String, Box<Expression>, Box<Expression>),
    Reference(String),
}

struct Function {
    assignments: Vec<(String, Expression)>,
    value: Expression,
}

fn parse_sized_axis(input: &str) -> IResult<&str, SizedAxis> {
    let (rest, (size, description)) = tuple((
        u64,
        parse_axis_description,
    ))(input)?;
    let size = if size == 0 {
        None
    } else {
        Some(size as usize)
    };
    Ok((rest, SizedAxis {
        size,
        desc: description,
    }))
}

fn parse_targeted_description(input: &str) -> IResult<&str, AxisDescription> {
    let (rest, subdescription) = delimited(
        tag("["),
        parse_sized_axis,
        tag("]"),
    )(input)?;
    Ok((rest, AxisDescription::Target(Box::new(subdescription))))
}

fn parse_compose_axis(input: &str) -> IResult<&str, AxisDescription> {
    let (rest, descriptions) = delimited(
        tag("("),
        separated_list1(
            space0,
            parse_sized_axis,
        ),
        tag(")"),
    )(input)?;
    Ok((rest, AxisDescription::Concatenation(descriptions)))
}

fn parse_concat_axis(input: &str) -> IResult<&str, AxisDescription> {
    let (rest, descriptions) = delimited(
        tag("["),
        separated_list1(
            tuple((space0, tag("+"), space0)),
            parse_sized_axis,
        ),
        tag("]"),
    )(input)?;
    Ok((rest, AxisDescription::Concatenation(descriptions)))
}

fn parse_axis_name(input: &str) -> IResult<&str, AxisDescription> {
    let (rest, name) = alpha1(input)?;
    Ok((rest, AxisDescription::Named(name.to_string())))
}

fn parse_wildcard(input: &str) -> IResult<&str, AxisDescription> {
    let (rest, _) = tag("_")(input)?;
    Ok((rest, AxisDescription::Wildcard))
}

fn parse_axis_description(input: &str) -> IResult<&str, AxisDescription> {
    alt((
        parse_wildcard,
        parse_axis_name,
        parse_concat_axis,
        parse_targeted_description,
        parse_compose_axis
    ))(input)
}

fn parse_axis_list(input: &str) -> IResult<&str, Vec<SizedAxis>> {
    let (rest, axis_list) = separated_list0(
        tuple((space0, tag(","), space0)),
        parse_sized_axis,
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
    Ok((rest, Expression::Ones(parse_result
                               .iter()
                               .map(|n| n.clone() as usize)
                               .collect())))
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

#[derive(Clone, Debug, PartialEq)]
pub struct Tensor {
    shape: Vec<usize>,
    values: Vec<f32>,
}

impl Tensor {
    fn from_shape_and_values(shape: &Vec<usize>, values: &Vec<f32>) -> Option<Tensor> {
        let expected_len = shape.iter().product();
        if values.len() != expected_len {
            None
        } else {
            Some(Tensor {
                shape: shape.clone(),
                values: values.clone(),
            })
        }
    }

    fn get(&self, index: Vec<usize>) -> Option<&f32> {
        if index.len() != self.shape.len() {
            return None
        };
        let raw_index: usize = self.indices_to_raw_index(index)?;
        return self.values.get(raw_index)
    }

    fn indices_to_raw_index(&self, indices: Vec<usize>) -> Option<usize> {
        let sizes: Option<Vec<usize>> = indices
            .iter()
            .map(|dim| { self.shape.get(*dim).map(|x| x.clone())})
            .collect();
        sizes.map(|sizes| sizes.iter().zip(indices).map(|(size, idx)| size * idx).sum())
    }

    fn raw_index_to_indices(&self, raw_index: usize) -> Option<Vec<usize>> {
        if raw_index > self.shape.iter().product() {
            return None
        };
        let total_sizes = self.shape
                            .iter()
                            .rev()
                            .foldmap::<usize, usize, usize, _>(1, |s, size| (s * size, s.clone()))
                            .collect::<Vec<usize>>()
                            .into_iter()
                            .rev();

        let indices = total_sizes
            .foldmap::<usize, usize, usize, _>(raw_index, |remaining, size| {
                let (this_idx, remaining) = divrem(*remaining, size);
                (remaining, this_idx)
            }).collect();
        Some(indices)
    }
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

// fn recursive_descent_interpret(program: Function) -> Tensor {
//     call(program, Environment::new_empty())
// }

// fn call(func: Function, env: Environment) -> Tensor {

// }



fn repeat(count: usize, value: f32) -> Vec<f32> {
    let mut v = Vec::new();
    v.resize(count, value);
    v
}

#[derive(Debug)]
enum InferenceError {
    InferenceNotImplemented
}

fn infer_sizes(ein: &Ein, input_sizes: Vec<usize>) -> Result<Ein, InferenceError> { // TODO actually implement this. currently just checks that no inference needs to be done
    fn has_missing_sizes(axis: &SizedAxis) -> bool {
        if let None = axis.size {
            true
        } else {
            match &axis.desc {
                AxisDescription::Composition(subaxes) => subaxes.iter().any(has_missing_sizes),
                _ => false,
            }
        }
    }

    if  ein.before.iter().chain(ein.after.iter()).any(has_missing_sizes) {
        Err(InferenceError::InferenceNotImplemented)
    } else {
        Ok(ein.clone())
    }
}

#[derive(Debug)]
enum RearrangeError {
    UndefinedAxis, UnderspecifiedSizes
}

pub fn rearrange(ein: &Ein, tensor: &Tensor) -> Result<Tensor, RearrangeError> {
    let ein = infer_sizes(&ein, tensor.shape.clone()).map_err(|_| RearrangeError::UnderspecifiedSizes)?;
    let flattened_ein = ein.flatten_compositions();
    let before_indices: HashMap<String, usize> = flattened_ein.before.iter().enumerate().filter_map(|(i, desc)| {
        if let AxisDescription::Named(name) = &desc.desc {
            Some((name.clone(), i))
        } else { None }
    }).collect();
    let mut mappings: HashMap<usize, usize> = HashMap::new();
    for (after_idx, desc) in flattened_ein.after.iter().enumerate() {
        if let AxisDescription::Named(name) = &desc.desc {
            if let Some(before_idx) = before_indices.get(name) {
                mappings.insert(after_idx, *before_idx);
            };
        };
    };

    let len = tensor.values.len();
    let result_shape = ein.after.iter() .map(|axis| axis.size) .collect(); // TODO: dim size inference

    let result_shape = if let Some(shape) = result_shape {
        shape
    } else {
        return Err(RearrangeError::UndefinedAxis)
    };

    let mut result = Tensor { values: Vec::with_capacity(len), shape: result_shape };
    for i in 0..tensor.values.len() {
        let new_indices = result.raw_index_to_indices(i).unwrap();
        let index_from_original: Vec<usize> = (0..tensor.shape.len())
            .map(|dim| new_indices.get(*mappings.get(&dim) .unwrap()) .unwrap().clone())
            .collect();
        let new_val: &f32 = tensor.get(index_from_original).unwrap();
        result.values.push(new_val.clone());
    };

    Ok(result)
}

// fn eval_expr(expr: Expression, env: Environment) -> Tensor {
//     match expr {
//         Expression::Ones(shape) => {
//             Tensor {
//                 shape,
//                 values: repeat(shape.iter().product(), 1.0),
//             }
//         },
//         Expression::Rearrange(ein, subexpr) => {
//             let subresult = eval_expr(*subexpr, env.clone());
//         },
//         Expression::Reduce(ein, func) => todo!(),
//         Expression::Map(_, _) => todo!(),
//         Expression::Reference(name) => todo!(),
//         Expression::Index(_, _, _) => todo!(),
//         Expression::Let(_, _, _) => todo!(),

//     }
// }

fn main() {
    println!("Hello, world!");
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn transpose() {
        let input = Tensor::from_shape_and_values(&vec![2, 3], &vec![1.0, 2.0,
                                                                     3.0, 4.0,
                                                                     5.0, 6.0]).unwrap();
        let einexpr = "2a 3b -> 3b 2a".to_string();
        let (_, parsed) = parse_ein(&einexpr).unwrap();
        let output = rearrange(&parsed, &input).unwrap();
        let expected = Tensor::from_shape_and_values(&vec![3, 2], &vec![1.0, 3.0, 5.0,
                                                                        2.0, 4.0, 6.0]).unwrap();
        assert_eq!(output, expected);
    }
}
