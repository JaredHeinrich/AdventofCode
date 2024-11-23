use std::{collections::BTreeSet, fs, u64};

#[derive(PartialEq, Eq)]
struct Conversion{
    destination_range_start: u64,
    source_range_start: u64,
    range_length: u64
}

impl PartialOrd for Conversion {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.source_range_start.partial_cmp(&other.source_range_start)
    }
}

impl Ord for Conversion {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.source_range_start.cmp(&other.source_range_start)
    }
}

struct Range{
    start: u64,
    length: u64,
}

type Map = BTreeSet<Conversion>;

fn main() {
    let input: String = fs::read_to_string("../input.txt").unwrap();
    let input: Vec<&str> = input.split("\n\n").collect();
    let seeds_str: &str = input[0];
    let map_strs: Vec<&str> = input[1..input.len()].to_vec();
    let seed_ranges: Vec<Range> = seeds_str
        .split(":")
        .nth(1)
        .unwrap()
        .trim()
        .split(" ")
        .map(|n| n.parse::<u64>().unwrap())
        .collect::<Vec<u64>>()
        .chunks(2)
        .map(|chunk| Range{start: chunk[0], length: chunk[1]})
        .collect();
    let maps: Vec<Map> = map_strs
        .iter()
        .map(|map_str|{
            map_str
                .split(":")
                .nth(1)
                .unwrap()
                .trim()
                .split("\n")
                .map(|line| {
                    let numbers: Vec<u64> = line
                        .split(" ")
                        .map(|n| n.parse::<u64>().unwrap())
                        .collect();
                    Conversion{
                        destination_range_start: numbers[0],
                        source_range_start: numbers[1],
                        range_length: numbers[2],
                    }
                }).collect()
        }).collect();

    let mut lowest: Option<u64> = None;
    for Range{start, length} in seed_ranges {
        let mut seed_iter = start..start+length;
        let mut possible_skips: usize = 0;
        while let Some(seed) = seed_iter.nth(possible_skips){
            let (location, skips) = get_location_and_skips(seed, &maps);
            lowest = match lowest{
                Some(current_value) if location >= current_value => lowest,
                _ => Some(location),
            };
            possible_skips = skips.try_into().unwrap();
        }

    }
    println!("lowest location: {:?}", lowest);
}

fn process_map(source: u64, map: &Map) -> (u64, u64){
    let mut next_conversion_change: u64 = u64::MAX;
    let mut destination = source;
    for Conversion{destination_range_start, source_range_start, range_length} in map {
        if source < *source_range_start {
            next_conversion_change = *source_range_start;
            break;
        }
        if source < *source_range_start + *range_length {
            destination = source - source_range_start + destination_range_start;
            next_conversion_change = source_range_start + range_length;
            break;
        }
    }
    let skips = next_conversion_change - source - 1;
    (destination, skips)
}

fn get_location_and_skips(seed: u64, maps: &Vec<Map>) -> (u64, u64){
    let mut current_pos = seed;
    let mut max_skips = None;
    for map in maps {
        let (next_pos, skips) = process_map(current_pos, map);
        current_pos = next_pos;
        max_skips = match max_skips {
            Some(current_value) if skips >= current_value => max_skips,
            _ => Some(skips),
        }
    }
    (current_pos, max_skips.unwrap_or(0))
}
