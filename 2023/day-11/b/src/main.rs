use core::str;
use std::{collections::BTreeSet, fs, usize};

fn main() {
    let input = fs::read_to_string("../input.txt").unwrap();
    let galaxy = Galaxy::new(&input);
    let mut distance_sum = 0;
    for a in 0..galaxy.stars.len(){
        for b in a..galaxy.stars.len(){
            distance_sum += galaxy.stars.get(a).unwrap().distance_to(galaxy.stars.get(b).unwrap());
        }
    }
    println!("Distance Sum: {}", distance_sum);
}

#[derive(Debug)]
struct Star{
    x: usize,
    y: usize,
}
impl Star {
    pub fn distance_to(&self, other: &Self) -> usize {
        self.x.abs_diff(other.x) + self.y.abs_diff(other.y)
    }
}

struct Galaxy{
    stars: Vec<Star>,
}

impl Galaxy{
    pub fn new(galaxy_str: &str) -> Self{
        let mut galaxy = Galaxy::from_str(galaxy_str);
        galaxy.expand();
        galaxy
    }

    fn from_str(galaxy_str: &str) -> Self{
        let mut stars = Vec::new();
        for (y, line) in galaxy_str.lines().enumerate() {
            for (x, char) in line.chars().enumerate() {
                if char == '#' {
                    stars.push(Star{x,y})
                }
            }
        }
        Self{stars}
    }

    fn expand(&mut self){
        let mut rows = BTreeSet::new();
        let mut columns = BTreeSet::new();
        for star in self.stars.iter(){
            rows.insert(star.y);
            columns.insert(star.x);
        }
        let last_star_column: usize = columns.pop_last().unwrap_or(0);
        let last_star_row: usize = rows.pop_last().unwrap_or(0);
        let empty_rows = (0..last_star_row).filter(|n| !rows.contains(n)).collect::<BTreeSet<usize>>();
        let empty_columns = (0..last_star_column).filter(|n| !columns.contains(n)).collect::<BTreeSet<usize>>();
        for row in empty_rows.iter().rev() {
            for star in self.stars.iter_mut() {
                if *row < star.y {
                    star.y += 999999;
                }
            }
        }
        for column in empty_columns.iter().rev() {
            for star in self.stars.iter_mut() {
                if *column < star.x {
                    star.x += 999999;
                }
            }
        }
    }
}
