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
        space0, space1,
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
    }, CompareResult
};

// result of parse
type PR<'a, O> = Result<(&'a str, O), nom::Err<nom::error::VerboseError<&'a str>>>;

#[derive(Clone, Debug, PartialEq)]
struct SizedAxis {
    desc: AxisDescription,
    size: Option<usize>,
}

#[derive(Clone, Debug, PartialEq)]
enum AxisDescription {
    Wildcard,
    Named(String),
    Target(Box<SizedAxis>),
    Composition(Vec<SizedAxis>),
    Concatenation(Vec<SizedAxis>)
}

#[derive(Clone, Debug, PartialEq)]
struct Ein {
    before: Vec<Vec<SizedAxis>>,
    after: Vec<Vec<SizedAxis>>
}

impl Ein {
    fn input_tensors_n(&self) -> u64 {
        self.before.len() as u64
    }

    fn output_tensors_n(&self) -> u64 {
        self.before.len() as u64
    }

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
            before: self.before.iter().map(|tensor| flatten_axis_descriptions(tensor)).collect(),
            after: self.after.iter().map(|tensor| flatten_axis_descriptions(tensor)).collect(),
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

fn parse_sized_axis(input: &str) -> PR<SizedAxis> {
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

fn parse_targeted_description(input: &str) -> PR<AxisDescription> {
    let (rest, subdescription) = delimited(
        tag("["),
        parse_sized_axis,
        tag("]"),
    )(input)?;
    Ok((rest, AxisDescription::Target(Box::new(subdescription))))
}

fn parse_compose_axis(input: &str) -> PR<AxisDescription> {
    let (rest, descriptions) = delimited(
        tag("("),
        separated_list1(
            space1,
            parse_sized_axis,
        ),
        tag(")"),
    )(input)?;
    Ok((rest, AxisDescription::Composition(descriptions)))
}

fn parse_concat_axis(input: &str) -> PR<AxisDescription> {
    let (rest_input, (first, _, rest_axes)) = tuple((
        parse_sized_axis, tuple((space0, tag("+"), space0)),
        separated_list1(
            tuple((space0, tag("+"), space0)),
            parse_sized_axis,
        )))(input)?;
    let mut all_descriptions = vec![first];
    all_descriptions.extend(rest_axes);
    Ok((rest_input, AxisDescription::Concatenation(all_descriptions)))
}

fn parse_axis_name(input: &str) -> PR<AxisDescription> {
    let (rest, name) = alpha1(input)?;
    Ok((rest, AxisDescription::Named(name.to_string())))
}

fn parse_wildcard(input: &str) -> PR<AxisDescription> {
    let (rest, _) = tag("_")(input)?;
    Ok((rest, AxisDescription::Wildcard))
}

fn parse_axis_description(input: &str) -> PR<AxisDescription> {
    alt((
        parse_wildcard,
        parse_axis_name,
        parse_concat_axis,
        parse_targeted_description,
        parse_compose_axis
    ))(input)
}

fn parse_axis_list(input: &str) -> PR<Vec<SizedAxis>> {
    let (rest, axis_list) = separated_list0(
        space1,
        parse_sized_axis,
    )(input)?;
    Ok((rest, axis_list))
}

fn parse_multiple_axis_lists(input: &str) -> PR<Vec<Vec<SizedAxis>>> {
    separated_list1(
        tuple((space0, tag(","), space0)),
        parse_axis_list)(input)
}

fn parse_ein(input: &str) -> PR<Ein> {
    let (rest, (before, after)) = separated_pair(
        parse_multiple_axis_lists,
        tuple((space0, tag("->"), space0)),
        parse_multiple_axis_lists,
    )(input)?;
    Ok((rest, Ein {
        before,
        after,
    }))
}

fn parse_reference(input: &str) -> PR<Expression> {
    let (rest, name) = alpha1(input)?;
    Ok((rest, Expression::Reference(name.to_string())))
}

fn parse_map(input: &str) -> PR<Expression> {
    let (rest, (ein, func)) = separated_pair(
        parse_ein,
        tag("/"),
        parse_function
    )(input)?;
    Ok((rest, Expression::Map(ein, Box::new(func))))
}

fn parse_reduce(input: &str) -> PR<Expression> {
    let (rest, (ein, func)) = separated_pair(
        parse_ein,
        tag("/"),
        parse_function
    )(input)?;
    Ok((rest, Expression::Reduce(ein, Box::new(func))))
}

fn parse_rearrange(input: &str) -> PR<Expression> {
    let (rest, (ein, expr)) = separated_pair(
        parse_ein,
        tag("#"),
        parse_expression
    )(input)?;
    Ok((rest, Expression::Rearrange(ein, Box::new(expr))))
}

fn parse_constant(input: &str) -> PR<Expression > {
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

fn parse_expression(input: &str) -> PR<Expression> {
    alt((
        parse_constant,
        parse_rearrange,
        parse_reduce,
        parse_map,
    ))(input)
}

fn parse_assignment(input: &str) -> PR<(String, Expression)> {
    let (rest, (lhs, rhs)) = separated_pair(
                alpha1,
                tuple((space0, tag("="), space0)),
                parse_expression,
            )(input)?;
    Ok((rest, (lhs.to_string(), rhs)))

}

fn parse_function(input: &str) -> PR<Function> {
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

    fn get_other_shape(&self, other_shape: &Vec<usize>, index: &Vec<usize>) -> Option<&f32> {
        let raw_index: usize = indices_to_raw_index(other_shape, index);
        self.values.get(raw_index)
    }

    fn get(&self, index: Vec<usize>) -> Option<&f32> {
        let raw_index: usize = self.indices_to_raw_index(index)?;
        self.values.get(raw_index)
    }

    fn cumulative_sizes(&self) -> Vec<usize> {
        cumulative_sizes(&self.shape)
    }

    fn indices_to_raw_index(&self, indices: Vec<usize>) -> Option<usize> {
        if self.shape.len() != indices.len() {
            return None
        };
        Some(indices_to_raw_index(&self.shape, &indices))
    }

    fn raw_index_to_indices(&self, raw_index: usize) -> Option<Vec<usize>> {
        if raw_index > self.shape.iter().product() {
            return None
        };
        Some(raw_index_to_indices(&self.shape, raw_index))
    }
}

fn cumulative_sizes(shape: &Vec<usize>) -> Vec<usize> {
    let total_sizes = shape
                          .iter()
                          .rev()
                          .foldmap::<usize, usize, usize, _>(1, |s, size| (s * size, s.clone()))
        .collect::<Vec<usize>>()
        .into_iter()
        .rev();
    total_sizes.collect()
}

fn raw_index_to_indices(shape: &Vec<usize>, raw_index: usize) -> Vec<usize> {
    let cumsizes = cumulative_sizes(shape);
    let indices = cumsizes.iter()
                          .foldmap::<usize, usize, usize, _>(raw_index, |remaining, size| {
                              let (this_idx, remaining) = divrem(*remaining, size);
                              (remaining, this_idx)
                          }).collect();
    indices
}

fn indices_to_raw_index(shape: &Vec<usize>, indices: &Vec<usize>) -> usize {
    let cumsizes = cumulative_sizes(shape);
    cumsizes.iter()
         .zip(indices)
         .map(|(cum_dim_len, idx)| {
             cum_dim_len * idx
         }).sum()
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

fn infer_sizes(ein: &Ein, input_sizes: &Vec<Vec<usize>>) -> Result<Ein, InferenceError> { // TODO actually implement this. currently just checks that no inference needs to be done
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

    if  ein.before.iter().chain(ein.after.iter()).any(|tensor_desc| tensor_desc.iter().any(has_missing_sizes)) {
        Err(InferenceError::InferenceNotImplemented)
    } else {
        Ok(ein.clone())
    }
}

#[derive(Debug)]
enum RearrangeError {
    UndefinedAxis, UnderspecifiedSizes,
    TooManyInputs, TooManyOutputs,
    MultipleInputsWithSingleName, SingleSource
}

#[derive(Debug)]
struct Source {
    tensor: usize,
    axes: Vec<AxisSource>,
}

#[derive(Debug, Clone)]
enum AxisSource {
    Direct(usize),
    Concatenation(Vec<usize>),
}

pub fn rearrange_multiple(ein: &Ein, tensors: &Vec<&Tensor>) -> Result<Vec<Tensor>, RearrangeError> {
    let ein = infer_sizes(&ein, &tensors.iter().map(|tensor| tensor.shape.clone()).collect()).map_err(|_| RearrangeError::UnderspecifiedSizes)?;
    let flattened_ein = ein.flatten_compositions();
    let before_indices: HashMap<String, (usize, usize)> = flattened_ein.before.iter().enumerate().map(|(tensor_i, tensor)| {
        tensor.iter().enumerate().filter_map(move |(axis_i, desc)| {
            match &desc.desc {
                AxisDescription::Named(name) => Some((name.clone(), (tensor_i, axis_i))),
                _ => None,
            }
        })}).flatten().collect();
    let mut sources: Vec<Source> = Vec::new();
    for (tensor_idx, tensor) in flattened_ein.after.iter().enumerate() {
        let mut axis_mappings: Vec<AxisSource> = Vec::new();
        let mut tensor_source_idx: Option<usize> = None;
        for (axis_idx, axis) in tensor.iter().enumerate() {
            match &axis.desc {
                AxisDescription::Named(name) => {
                    if let Some((before_tensor_idx, before_axis_idx)) = before_indices.get(name) {
                        if let Some(previously_assigned_source) = tensor_source_idx {
                            if previously_assigned_source != *before_tensor_idx {
                                return Err(RearrangeError::SingleSource);
                            }
                        } else {
                            tensor_source_idx = Some(*before_tensor_idx);
                        }
                        axis_mappings.push(AxisSource::Direct(*before_axis_idx));
                    } else {
                        return Err(RearrangeError::UndefinedAxis);
                    }
                }
                AxisDescription::Concatenation(subaxes) => todo!(),
                _ => (),
            };
        }
        sources.push(Source {
            tensor: tensor_source_idx.ok_or(RearrangeError::UndefinedAxis)?,
            axes: axis_mappings.clone(),
        });
    };

    fn get_shapes(tensor_descriptions: &Vec<Vec<SizedAxis>>) -> Result<Vec<Vec<usize>>, RearrangeError> {
        tensor_descriptions.iter()
                                .map(|tensor| {
                                    tensor.iter()
                                          .map(|axis| axis.size.ok_or(RearrangeError::UndefinedAxis))
                                          .collect::<Result<Vec<usize>, RearrangeError>>()
                                })
                                .collect::<Result<Vec<Vec<usize>>, RearrangeError>>()

    }

    let reinterpreted_input_shapes = get_shapes(&flattened_ein.before)?;
    let flattened_result_shapes = get_shapes(&flattened_ein.after)?;
    let result_shapes = get_shapes(&ein.after)?;
        // TODO: dim size inference

    let mut result: Vec<Tensor> = result_shapes.iter().map(|shape| {
        let len = shape.iter().product();
        Tensor { values: Vec::with_capacity(len), shape: shape.to_vec() }
    }).collect();

    for result_tensor_idx in 0..result.len() {
        println!("result_tensor_idx: {}", result_tensor_idx);
        let result_tensor: &mut Tensor = &mut result[result_tensor_idx];
        let source_desc: &Source = &sources[result_tensor_idx];
        let source_tensor: &Tensor = &tensors[source_desc.tensor];
        let result_shape: &Vec<usize> = &flattened_result_shapes[result_tensor_idx];
        let axis_sources: &Vec<AxisSource> = &source_desc.axes;
        for i in 0..result_tensor.shape.iter().product() {
            println!("i: {}", i);
            let index_in_result = raw_index_to_indices(result_shape, i);
            println!("index_in_result: {:?}", index_in_result);
            let index_from_original: Vec<usize> = (0..source_tensor.shape.len())
                .map(|dim| index_in_result.get(dim).unwrap().clone())
                .collect();
            println!("index_from_original: {:?}", index_from_original);
            let new_val: &f32 = source_tensor.get_other_shape(&reinterpreted_input_shapes[source_desc.tensor], &index_from_original).unwrap();
            result_tensor.values.push(new_val.clone());
        }
    }

    Ok(result)
}

pub fn rearrange(ein: &Ein, tensor: &Tensor) -> Result<Tensor, RearrangeError> {
    if ein.input_tensors_n() > 1 {
        return Err(RearrangeError::TooManyInputs);
    }
    else if ein.output_tensors_n() > 1 {
        return Err(RearrangeError::TooManyOutputs);
    }
    Ok(rearrange_multiple(ein, &vec![tensor])?.get(0).unwrap().clone())
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
    fn indices_to_raw_sizes() {
        let indices = vec![1, 0];
        let tensor = Tensor::from_shape_and_values(&vec![2, 3], &vec![1.0, 2.0, 3.0, 4.0, 5.0, 6.0]).unwrap();
        let result = tensor.indices_to_raw_index(indices);

        assert_eq!(result, Some(3));
    }

    #[test]
    fn get() {
        let tensor = Tensor::from_shape_and_values(&vec![2, 3], &vec![1.0, 2.0, 3.0, 4.0, 5.0, 6.0]).unwrap();
        let result = tensor.get(vec![1, 0]).unwrap();

        assert_eq!(result, &4.0);
    }

    #[test]
    fn parse_sized_named() {
        let string = "2a".to_string();
        let (rest, parsed) = parse_sized_axis(&string).unwrap();

        assert_eq!(rest, "");
        assert_eq!(parsed, SizedAxis{
            size: Some(2),
            desc: AxisDescription::Named("a".to_string()),
        });
    }

    #[test]
    fn parse_axis_list_2_sized() {
        let string = "2a 3b".to_string();
        let (rest, parsed) = parse_axis_list(&string).unwrap();

        assert_eq!(rest, "");
        assert_eq!(parsed, vec![
            SizedAxis{
                size: Some(2),
                desc: AxisDescription::Named("a".to_string()),
            },
            SizedAxis{
                size: Some(3),
                desc: AxisDescription::Named("b".to_string()),
            },
        ]);
    }

    #[test]
    fn parse_transpose_ein() {
        let einexpr = "2a 3b -> 3b 2a".to_string();
        let (rest, parsed) = parse_ein(&einexpr).unwrap();

        assert_eq!(rest, "");
        assert_eq!(parsed, Ein {
            before: vec![vec![
                SizedAxis {size: Some(2), desc: AxisDescription::Named("a".to_string())},
                SizedAxis {size: Some(3), desc: AxisDescription::Named("b".to_string())},
            ]],
            after: vec![vec![
                SizedAxis {size: Some(3), desc: AxisDescription::Named("b".to_string())},
                SizedAxis {size: Some(2), desc: AxisDescription::Named("a".to_string())},
            ]],
        })
    }

    #[test]
    fn transpose_2_to_3() {
        let input = Tensor::from_shape_and_values(&vec![2, 3], &vec![1.0, 2.0, 3.0,
                                                                     4.0, 5.0, 6.0]).unwrap();
        let einexpr = "2a 3b -> 3b 2a".to_string();
        let (rest, parsed) = parse_ein(&einexpr).unwrap();
        assert_eq!(rest, "");
        let output = rearrange(&parsed, &input).unwrap();
        let expected = Tensor::from_shape_and_values(&vec![3, 2], &vec![1.0, 4.0,
                                                                        2.0, 5.0,
                                                                        3.0, 6.0]).unwrap();
        assert_eq!(output, expected);
    }

    #[test]
    fn transpose_3_to_2() {
        let input = Tensor::from_shape_and_values(&vec![3, 2], &vec![1.0, 2.0,
                                                                     3.0, 4.0,
                                                                     5.0, 6.0]).unwrap();
        let einexpr = "3a 2b -> 2b 3a".to_string();
        let (_, parsed) = parse_ein(&einexpr).unwrap();
        let output = rearrange(&parsed, &input).unwrap();
        let expected = Tensor::from_shape_and_values(&vec![2, 3], &vec![1.0, 3.0, 5.0,
                                                                        2.0, 4.0, 6.0]).unwrap();
        assert_eq!(output, expected);
    }

    #[test]
    fn transpose_outer_and_middle_3d() {
        let input = Tensor::from_shape_and_values(&vec![3, 2, 2], &vec![1.0,  2.0,  3.0, 4.0,
                                                                        5.0,  6.0,  7.0, 8.0,
                                                                        9.0, 10.0, 11.0, 12.0]).unwrap();
        let einexpr = "3a 2b 2i -> 2b 3a 2i".to_string();
        let (_, parsed) = parse_ein(&einexpr).unwrap();
        let output = rearrange(&parsed, &input).unwrap();
        let expected = Tensor::from_shape_and_values(&vec![2, 3, 2], &vec![1.0, 2.0, 5.0, 6.0,  9.0, 10.0,
                                                                           3.0, 4.0, 7.0, 8.0, 11.0, 12.0]).unwrap();
        assert_eq!(output, expected);

    }

    #[test]
    fn transpose_outer_and_inner_3d() {
        let input = Tensor::from_shape_and_values(&vec![3, 2, 2], &vec![1.0,  2.0,  3.0, 4.0,
                                                                        5.0,  6.0,  7.0, 8.0,
                                                                        9.0, 10.0, 11.0, 12.0]).unwrap();
        let einexpr = "3a 2m 2b -> 2b 2m 3a".to_string();
        let (_, parsed) = parse_ein(&einexpr).unwrap();
        let output = rearrange(&parsed, &input).unwrap();
        let expected = Tensor::from_shape_and_values(&vec![2, 2, 3], &vec![1.0, 5.0,  9.0, 3.0, 7.0, 11.0,
                                                                           2.0, 6.0, 10.0, 4.0, 8.0, 12.0]).unwrap();
        assert_eq!(output, expected);
    }

    #[test]
    fn parse_compose() {
        let einexpr = "6(3a 2b)";
        let (rest, parsed) = parse_sized_axis(einexpr).unwrap();
        let expected = SizedAxis {
            size: Some(6),
            desc: AxisDescription::Composition(vec![
                SizedAxis {
                    size: Some(3),
                    desc: AxisDescription::Named("a".to_string()),
                },
                SizedAxis {
                    size: Some(2),
                    desc: AxisDescription::Named("b".to_string()),
                }
            ])
        };
        assert_eq!(rest, "");
        assert_eq!(parsed, expected);
    }

    #[test]
    fn split_axes() {
        let input = Tensor::from_shape_and_values(&vec![6], &vec![1.0, 2.0, 3.0, 4.0, 5.0, 6.0]).unwrap();
        let einexpr = "6(3a 2b) -> 3a 2b";
        let (_, parsed) = parse_ein(einexpr).unwrap();
        let output = rearrange(&parsed, &input).unwrap();
        let expected = Tensor::from_shape_and_values(&vec![3, 2], &vec![1.0, 2.0, 3.0, 4.0, 5.0, 6.0]).unwrap();
        assert_eq!(output, expected);
    }

    #[test]
    fn join_axes() {
        let input = Tensor::from_shape_and_values(&vec![3, 2], &vec![1.0, 2.0, 3.0, 4.0, 5.0, 6.0]).unwrap();
        let einexpr = "3a 2b -> 6(3a 2b)";
        let (_, parsed) = parse_ein(einexpr).unwrap();
        let output = rearrange(&parsed, &input).unwrap();
        let expected = Tensor::from_shape_and_values(&vec![6], &vec![1.0, 2.0, 3.0, 4.0, 5.0, 6.0]).unwrap();
        assert_eq!(output, expected);
    }

    #[test]
    fn join_and_transpose() {
        let input = Tensor::from_shape_and_values(&vec![3, 2], &vec![1.0, 2.0,
                                                                     3.0, 4.0,
                                                                     5.0, 6.0]).unwrap();
        let einexpr = "3a 2b -> 6(2b 3a)";
        let (_, parsed) = parse_ein(einexpr).unwrap();
        let output = rearrange(&parsed, &input).unwrap();
        let expected = Tensor::from_shape_and_values(&vec![6], &vec![1.0, 3.0, 5.0, 2.0, 4.0, 6.0]).unwrap();
        assert_eq!(output, expected);
    }

    #[test]
    fn concat() {
        let input1 = Tensor::from_shape_and_values(&vec![2, 3], &vec![1.0, 2.0, 3.0,
                                                                      4.0, 5.0, 6.0]).unwrap();
        let input2 = Tensor::from_shape_and_values(&vec![3], &vec![7.0, 8.0, 9.0]).unwrap();
        let einexpr = "2a 3b, 3c -> 3a 3(3b + 3c)";
        let (rest, parsed) = parse_ein(einexpr).unwrap();
        assert_eq!(rest, "");
        let output = rearrange_multiple(&parsed, &vec![&input1, &input2]).unwrap();
        let expected = vec![Tensor::from_shape_and_values(&vec![3, 3], &vec![1.0, 2.0, 3.0,
                                                                            4.0, 5.0, 6.0,
                                                                            7.0, 8.0, 9.0,]).unwrap()];
        assert_eq!(output, expected);
    }
}
