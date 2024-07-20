use std::{collections::HashSet, fs, u32, usize};

fn main() {
    let input = fs::read_to_string("../input.txt").unwrap();
    let mut cards: Vec<(u32, u32, usize)> = input.lines().map(|card|{
        let card: &str = card.split(": ").nth(1).unwrap();
        let card: Vec<&str> = card.split("|").collect();
        let matching = str_to_numbers(card[0]).intersection(&str_to_numbers(card[1])).count();
        (1,1,matching)
    }).collect();

    let mut won: bool = true;
    let card_len = cards.len();
    while won {
        won = false;
        for index in 0..card_len {
            let number_of_cards: u32 = cards[index].1;
            if number_of_cards <= 0 {
                continue;
            }
            won = true;
            cards[index].1 = 0;
            let matches = cards[index].2;
            let end_index = index + matches;
            for i in index+1..=end_index {
                let (counter, number, _) = &mut cards[i];
                *number += number_of_cards; 
                *counter += number_of_cards;
            }
        }
    }
    let mut result = 0;
    for (i, (c,_,_)) in cards.iter().enumerate() {
        println!("Card {}: {}",i+1,c);
        result += c;
    }
    println!("result: {}",result);
}

fn str_to_numbers(string: &str) -> HashSet<u32>{
    string.split(' ').filter(|s|{!s.is_empty()}).map(|i|{i.parse().unwrap()}).collect()
}
