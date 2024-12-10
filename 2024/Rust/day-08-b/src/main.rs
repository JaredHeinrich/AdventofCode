use std::{collections::{HashMap, HashSet}, fs, mem::swap, path::Path};

fn ggt(a: i32, b: i32) -> i32 {
    let mut a = a.abs();
    let mut b = b.abs();
    if a == b {return a;}
    if a == 0 {return b;}
    if b == 0 {return a;}
    let mut k = 0;
    while ((a | b) & 1) == 0 {
        a >>= 1;
        b >>= 1;
        k+=1;
    }
    while (a & 1) == 0 {a >>= 1}
    loop {
        while (b & 1) == 0 {b >>= 1}
        if a > b {swap(&mut a, &mut b);}
        b = b - a;
        if b == 0 {break;}
    }
    a << k
}

#[derive(PartialEq, Eq, Hash, Clone)]
struct Vector {
    x: i32,
    y: i32,
}

impl Vector {
    fn plus(&self, other: &Self) -> Self {
        let x = self.x + other.x;
        let y = self.y + other.y;
        Self{x,y}
    }
    fn minus(&self, other: &Self) -> Self {
        let x = self.x - other.x;
        let y = self.y - other.y;
        Self{x,y}
    }
    fn normalize(&self) -> Self {
        let ggt = ggt(self.x, self.y);
        let x = self.x / ggt;
        let y = self.y / ggt;
        Self{x,y}
    }
}

struct City {
    size: i32,
    antennas: HashMap<char, Vec<Vector>>
}

impl City {
    fn from_str(input: &str) -> Self {
        let size = input.lines().count() as i32;
        let antennas = input
            .lines()
            .enumerate()
            .flat_map(|(y,line)|{
                line
                    .chars()
                    .enumerate()
                    .filter(|(_,c)| *c != '.')
                    .map(move |(x,c)|{(c,Vector{x: x as i32, y: y as i32})})
            })
            .fold(HashMap::new(), |mut antennas, (c, vector)|{
                antennas.entry(c).or_insert(Vec::new()).push(vector);
                antennas
            });
        City{size,antennas}
    }

    fn calculate_antinodes(&self, antenna_one: &Vector, antenna_two: &Vector) -> Vec<Vector> {
        let mut result = Vec::new();
        let mut current_pos: Vector = antenna_one.clone();
        let normalized_one_to_two = antenna_two.minus(antenna_one).normalize();
        while self.vector_is_in_city_range(&current_pos) {
            result.push(current_pos.clone());
            current_pos = current_pos.plus(&normalized_one_to_two);
        }
        current_pos = antenna_one.clone();
        while self.vector_is_in_city_range(&current_pos) {
            result.push(current_pos.clone());
            current_pos = current_pos.minus(&normalized_one_to_two);
        }
        result
    }

    fn calculate_all_antinodes(&self) -> HashSet<Vector> {
        let mut antinodes = HashSet::new();
        for (_, antennas) in self.antennas.iter() {
            for i in 0..antennas.len()-1 {
                for j in i+1..antennas.len() {
                    let nodes = self.calculate_antinodes(antennas.get(i).unwrap(), antennas.get(j).unwrap());
                    for node in nodes.into_iter() {
                        antinodes.insert(node);
                    }
                }
            }
        }
        antinodes
    }

    fn vector_is_in_city_range(&self, vector: &Vector) -> bool {
        vector.x >= 0 && vector.y >= 0 && vector.x < self.size && vector.y < self.size
    }
}

fn solve(city: &City) -> i32 {
    city.calculate_all_antinodes()
        .iter()
        .count() as i32
}

fn main() {
    let filepath = Path::new("../../inputs/input-08.txt");
    let input: String = fs::read_to_string(filepath).unwrap();
    let city = City::from_str(&input);
    let solution = solve(&city);
    println!("solution: {}", solution);
}
