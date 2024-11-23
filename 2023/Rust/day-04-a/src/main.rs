use std::{collections::HashSet, fs, u32};

fn main() {
    let input = fs::read_to_string("../../inputs/input-04.txt").unwrap();
    let cards: Vec<(HashSet<u32>, HashSet<u32>)> = input.lines().map(|card|{
        let card: &str = card.split(": ").nth(1).unwrap();
        let card: Vec<&str> = card.split("|").collect();
        (str_to_numbers(card[0]), str_to_numbers(card[1]))
    }).collect();
    let mut points = 0;
    for (w, m) in cards.iter(){
        let intersections:u32 = m.intersection(w).count() as u32;
        if intersections > 0 {
            points += (2 as u32).pow(intersections-1);
        }
    }
    println!("points: {}", points);
}

fn str_to_numbers(string: &str) -> HashSet<u32>{
    string.split(' ').filter(|s|{!s.is_empty()}).map(|i|{i.parse().unwrap()}).collect()
}
