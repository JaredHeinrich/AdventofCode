use std::{fs, process::exit, collections::HashMap};
fn main() {
    let mut max_values: HashMap<&str, u32> = HashMap::new();
    max_values.insert("red",12);
    max_values.insert("blue",14);
    max_values.insert("green",13);
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
    let mut counter: u32 = 0;
    for (index, game) in lines.iter().enumerate() {
        let mut farben: HashMap<&str, u32> = HashMap::new();
        for round in game.iter() {
            for (color, number)in round.iter() {
                let value = farben.entry(color).or_insert(0);
                if *number > *value {
                    *value = *number;
                }
            }
        }
        println!("Game: {}", index+1);
        println!("{:?}",farben);
        let possible: bool = max_values.iter().all(|(key, &value_1)| {
            farben.get(key).map_or(true, |&value_2|  {
                value_1 >= value_2
            })
        });
        println!("Possible: {}", possible);
        if possible {
            counter += index as u32 + 1;
        }
    }
    println!("{}", counter);
}
