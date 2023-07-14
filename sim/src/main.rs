use rand::prelude::*;

struct World<const size: usize> {
    heights: [[u16; size]; size],
    continents: [[u8; size]; size],
}

fn man_distance(q1: usize, r1: usize, q2: usize, r2: usize) -> usize {
    let s1 = -q1 - r1;
    let s2 = -q2 - r2;
    max((q1 - q2).abs(), (r1 - r2).abs(), (s1 - s2).abs())
}

impl<const size: usize> World<size> {
    fn new() -> Self {
        World {
            heights: [[0; size]; size],
            continents: [[0; size]; size],
        }
    }

    fn gen_continents(&mut self, rng: &mut ThreadRng, count: u8) {
        let centers = (0..count)
            .map(|_| (rng.gen_range(0..size), rng.gen_range(0..size)))
            .collect::<Vec<_>>();
        for (idx, (x, y)) in centers.iter().enumerate() {
            self.continents[*x][*y] = idx as u8;
        }
    }

    fn generate(&mut self) {
        let mut rng = thread_rng();
        self.gen_continents(&mut rng);
    }
}
