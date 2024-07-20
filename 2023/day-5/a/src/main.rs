use std::{fs, u64};

type Map = Vec<(u64,u64,u64)>;

fn main() {
    let input: String = fs::read_to_string("../input.txt").unwrap();
    let input: Vec<&str> = input.split("\n\n").collect();
    let seeds_str: &str = input[0];
    let map_strs: Vec<&str> = input[1..input.len()].to_vec();
    let seeds: Vec<u64> = seeds_str
        .split(":")
        .nth(1)
        .unwrap()
        .trim()
        .split(" ")
        .map(|n| n.parse::<u64>().unwrap())
        .collect();
    let mut maps: Vec<Map> = Vec::new();
    for map_str in map_strs {
        maps.push(str_to_map(map_str));
    }
    let mut lowest: Option<u64> = None;
    for seed in seeds {
        let location = seed.get_location(&maps);
        if let Some(v) = lowest {
            if location < v {
                lowest = Some(location);
            }
        }else {
            lowest = Some(location);
        }
    }
    println!("lowest location: {:?}", lowest);
}

fn str_to_map(map_str: &str) -> Map{
    map_str.split(":").nth(1).unwrap().trim().split("\n").map(|line| {
        let numbers: Vec<u64> = line.split(" ").map(|n| n.parse::<u64>().unwrap()).collect();
        (numbers[0], numbers[1], numbers[2])
    }).collect()
}

impl N for u64 {
    fn is_in_range(&self, start: u64, range: u64) -> bool{
        *self >= start && *self < start+range
    }
    fn get_destination(&self, map: &Map) -> u64{
        let mut destination = *self;
        for (d,s,r) in map {
            if !self.is_in_range(*s, *r) {
                continue;
            }
            println!("{} {} {}", d, *self, s);
            destination = *self-s+d;
            break;
        }
        destination
    }
    fn get_location(&self, maps: &Vec<Map>) -> u64{
        let mut tmp_dest = *self;
        for m in maps {
            tmp_dest = tmp_dest.get_destination(m);
        }
        tmp_dest
    }
}

trait N {
    fn is_in_range(&self, start: u64, range: u64) -> bool;
    fn get_destination(&self, map: &Map) -> u64;
    fn get_location(&self, maps: &Vec<Map>) -> u64;
}
