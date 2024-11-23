use std::{fs, process::exit, collections::HashMap};
fn main() {
    let input = match fs::read_to_string("../../inputs/input-02.txt") {
        Ok(v) => v,
        Err(_) => {
            println!("fehler beim lesen");
            exit(0);
        },
    };
    let lines: Vec<Vec<Vec<(&str, u32)>>> = input.trim().split("\n")
        .map(|game| {
            let game: Vec<&str> = game.split(":").collect();
            let game = match game.get(1) {
                Some(v) => v,
                None => {
                    println!("error");
                    exit(0);
                },
            };
            game.split(";")
                .map(|round| {
                    round.split(",")
                        .map(|field| {
                            let field = field.trim();
                            let field: Vec<&str> = field.split_whitespace().collect();
                            (field[1], field[0].parse::<u32>().unwrap())
                        }).collect()
                }).collect()
        }).collect();
    let mut sum: u32 = 0;
    for (index, game) in lines.iter().enumerate() {
        let mut colors: HashMap<&str, u32> = HashMap::new();
        colors.insert("red",0);
        colors.insert("blue",0);
        colors.insert("green",0);
        for round in game.iter() {
            for (color, number)in round.iter() {
                let value = colors.entry(color).or_insert(0);
                if *number > *value {
                    *value = *number;
                }
            }
        }
        let mut power = 1;
        for value in colors.values() {
            power *= value;
        }
        sum += power;
        println!("Game: {}", index+1);
        println!("{:?}",colors);
        println!("{}", power);
    }
    println!("Summe: {}", sum);
}
