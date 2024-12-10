use std::{collections::{HashMap, HashSet}, fs, path::Path};

#[derive(PartialEq, Eq, Hash)]
struct Vector {
    x: i32,
    y: i32,
}

impl Vector {
    fn minus(&self, other: &Self) -> Self {
        let x = self.x - other.x;
        let y = self.y - other.y;
        Self{x,y}
    }

    fn times(&self, factor: i32) -> Self {
        let x = self.x * factor;
        let y = self.y * factor;
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

    fn calculate_antinodes(&self) -> HashSet<Vector> {
        let mut antinodes = HashSet::new();
        for (_, antennas) in self.antennas.iter() {
            for pos_antenna_one in antennas.iter(){
                for pos_antenna_two in antennas.iter(){
                    if pos_antenna_one == pos_antenna_two {continue;}
                    antinodes.insert(pos_antenna_one.times(2).minus(pos_antenna_two));
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
    city.calculate_antinodes()
        .iter()
        .filter(|v| city.vector_is_in_city_range(v))
        .count() as i32
}

fn main() {
    let filepath = Path::new("../../inputs/input-08.txt");
    let input: String = fs::read_to_string(filepath).unwrap();
    let city = City::from_str(&input);
    let solution = solve(&city);
    println!("solution: {}", solution);
}
