use std::{fs, ops::Div};

fn main() {
    let input: String = fs::read_to_string("../../inputs/input-06.txt").unwrap();
    let races: Vec<u64> = input
        .lines()
        .map(|line| line
            .split(":")
            .nth(1)
            .unwrap()
            .trim()
            .chars()
            .filter(|e|!(*e == ' '))
            .collect::<String>()
            .parse::<u64>()
            .unwrap()
        ).collect();
    println!("result: {}", one(&races));
}

fn one(races: &Vec<u64>) -> u64{
    let (l,u) = calculate_bounds(races[0], races[1]);
    u-l+1
}

fn calculate_bounds(time: u64, distance: u64) -> (u64, u64){
    let tmp:f64 = ((time.pow(2)-4*distance) as f64).sqrt();
    let lower: u64 = ((time as f64 - tmp).div(2.0)+1.0).floor() as u64;
    let upper: u64 = ((time as f64 + tmp).div(2.0)-1.0).ceil() as u64;
    (lower, upper)
}
