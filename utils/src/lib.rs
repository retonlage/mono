#![feature(type_alias_impl_trait)]
use std::{ops::{Div, Rem}};

pub fn divrem<A, B>(upper: A, lower: B) -> (<A as Div<B>>::Output, <A as Rem<B>>::Output)
    where
    A: Copy + Rem<B> + Div<B>,
    B: Copy
{
    (upper / lower, upper % lower)
}

pub struct FoldMap<I, A, B, S, F>
    where
        I: Iterator<Item=A>,
        F: FnMut(&S, A) -> (S, B)
{
    in_iter: I,
    current: S,
    f: F
}

impl<I, A, B, S, F> FoldMap<I, A, B, S, F>
    where
        I: Iterator<Item=A>,
        F: FnMut(&S, A) -> (S, B)

{
    fn new(in_iter: I, init: S, f: F) -> Self
        where I: Iterator<Item=A>
    {
        FoldMap {
            in_iter,
            current: init,
            f
        }
    }
}

impl<I, A, B, S, F> Iterator for FoldMap<I, A, B, S, F>
    where
        I: Iterator<Item=A>,
        F: FnMut(&S, A) -> (S, B)
{
    type Item = B;

    fn next(&mut self) -> Option<Self::Item> {
        let (new_state, res) = (self.f)(&self.current, self.in_iter.next()?);
        self.current = new_state;
        Some(res)
    }
}

pub trait IteratorExt: Iterator {
    fn foldmap<A, B, S, F>(self, init: S, f: F) -> FoldMap<Self, <Self as Iterator>::Item, B, S, F>
    where F: FnMut(&S, Self::Item) -> (S, B),
          Self: Sized
    {
        FoldMap::new(self, init, f)
    }
}

impl<I: Iterator> IteratorExt for I {}
