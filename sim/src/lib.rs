use std::ops::{Index, IndexMut};
use rand::prelude::*;

#[derive(Clone)]
struct ForTile<T: Clone> {
    world: Box<[[T; 500]; 500]>,
}

impl<T: Clone> ForTile<T> {
    fn get(&self, x: usize, y: usize) -> T {
        self.world[x][y].clone()
    }

    fn get_ref(&self, x: usize, y: usize) -> &T {
        &self.world[x][y]
    }

    fn set(&mut self, x: usize, y: usize, value: T) {
        self.world[x][y] = value;
    }

    fn adj(&self, x: usize, y: usize) -> [(usize, usize); 4] {
        [
            if x != 0   { (x - 1, y) } else { (499, y  ) },
            if x != 499 { (x + 1, y) } else { (0  , y  ) },
            if y != 0   { (x, y - 1) } else { (x  , 499) },
            if y != 499 { (x, y + 1) } else { (x  , 0  ) },
        ]
    }

    fn flat_iter(&self) -> impl Iterator<Item = &T> {
        self.world.iter().flat_map(|row| row.iter())
    }
}

impl<T: Clone> Index<usize> for ForTile<T> {
    type Output = [T; 500];

    fn index(&self, index: usize) -> &Self::Output {
        &self.world[index]
    }
}

impl<T: Clone> IndexMut<usize> for ForTile<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.world[index]
    }
}

type Heights = ForTile<f32>;

fn init_random_height<R: Rng>(rng: &mut R) -> ForTile<f32> {
    let mut world = Box::new([[0.0; 500]; 500]);
    for i in 0..500 {
        for j in 0..500 {
            world[i][j] = rng.gen::<f32>();
        }
    };
    Heights { world }
}

fn do_one_erosion(initial: &Heights) -> Heights {
    let mut sorted_idxs = initial.world
                                 .iter()
                                 .map(|row| row.iter().enumerate())
                                 .enumerate()
                                 .flat_map(|(i, row)| row.into_iter().map(move |(j, v)| (i, j, v)))
                                 .collect::<Vec<_>>();
    sorted_idxs.sort_by(|(_, _, a), (_, _, b)| a.partial_cmp(b).unwrap());
    let mut flows = Box::new([[1.0; 500]; 500]);
    let mut result = initial.clone();
    for (x, y, v) in sorted_idxs {
        let flow = flows[x][y];
        let adj = result.adj(x, y);
        let diffs = adj.map(|(ax, ay)| result.get(ax, ay) - result.get(x, y));
        let diffs_sum = diffs.iter().sum::<f32>();
        let diffs_normalized = diffs.map(|d| d / diffs_sum);
        result[x][y] -= flow;
        for (i, (ax, ay)) in adj.iter().enumerate() {
            let d = diffs_normalized[i];
            flows[*ax][*ay] += flow * d;
            result[*ax][*ay] += flow * d;
        };
    };
    result
}

trait Display {
    fn display(&self);
}

impl Display for Heights {
    fn display(&self) {
        let min = self.flat_iter().min_by(|a, b| a.partial_cmp(b).unwrap()).unwrap();
        let max = self.flat_iter().max_by(|a, b| a.partial_cmp(b).unwrap()).unwrap();
        let range = max - min;
        let mut image = image::ImageBuffer::new(500, 500);
        for (x, y, pixel) in image.enumerate_pixels_mut() {
            let v = self.get(x as usize, y as usize);
            let v = (v - min) / range;
            let v = (v * 255.0) as u8;
            *pixel = image::Rgb([v, v, v]);
        };
        image.save("erosion.png").unwrap();
    }
}

fn main() {
    println!("Hello, world!");
}
