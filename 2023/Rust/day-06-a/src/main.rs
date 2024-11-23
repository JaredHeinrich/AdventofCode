use core::f32;
use std::{fs, ops::Div, u32};

fn main() {
    let input: String = fs::read_to_string("../../inputs/input-06.txt").unwrap();
    let races: Vec<Vec<u32>> = input
        .lines()
        .map(|line| line
            .split(":")
            .nth(1)
            .unwrap()
            .trim()
            .split(" ")
            .filter(|e|!e.is_empty())
            .map(|n|n.parse::<u32>().unwrap())
            .collect())
        .collect();
    println!("result: {}", one(&races));
}

fn one(races: &Vec<Vec<u32>>) -> u32{
    let number_races = races[0].len();
    let mut result = 1;
    for i in 0..number_races {
        let (l,u) = calculate_bounds(races[0][i], races[1][i]);
        result = result*(u-l+1);
    }
    result
}

fn calculate_bounds(time: u32, distance: u32) -> (u32, u32){
    let tmp:f32 = ((time.pow(2)-4*distance) as f32).sqrt();
    let lower: u32 = ((time as f32 - tmp).div(2.0)+1.0).floor() as u32;
    let upper: u32 = ((time as f32 + tmp).div(2.0)-1.0).ceil() as u32;
    (lower, upper)
}
