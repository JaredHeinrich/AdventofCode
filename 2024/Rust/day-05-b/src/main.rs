use std::{fs, ops::Div, path::Path};

type Rule = (i32, i32);
type Update = Vec<i32>;

fn get_middle<T> (vector: &Vec<T>) -> Result<&T,&'static str> {
    let length = vector.len();
    if length % 2 == 0 {
        let error_message: &'static str = "Length of vector is even";
        return Err(error_message);
    }
    Ok(&vector[vector.len().div(2)])
}

fn is_update_ordered (rules: &Vec<Rule>, update: &Update) -> bool {
    rules.iter().all(|rule| rule_applies(rule, update))
}

fn update_from_str (str: &str) -> Update {
    str.split(",").map(|s| s.parse().unwrap()).collect()
}

fn rule_from_str (str: &str) -> Rule {
    let mut rule_iter = str.split("|").map(|s| s.parse().unwrap());
    (rule_iter.next().unwrap(), rule_iter.next().unwrap())

}

fn parse(input: &str) -> (Vec<Rule>, Vec<Update>) {
    let mut blocks = input.split("\n\n");
    let rules = blocks
        .next()
        .unwrap()
        .lines()
        .map(|s| rule_from_str(s))
        .collect();
    let updates = blocks
        .next()
        .unwrap()
        .lines()
        .map(|s| update_from_str(s))
        .collect();
    (rules, updates)
}

fn rule_applies((first, second): &Rule, update: &Update) -> bool {
    let opt_first_index = update.iter().position(|i| i == first);
    let opt_second_index = update.iter().position(|i| i == second);
    if opt_first_index.is_none() || opt_second_index.is_none() {
        return true;
    }
    opt_first_index.unwrap() < opt_second_index.unwrap()
}

fn sort_update(rules: &Vec<Rule>, update: &mut Update) {
    let mut sorted = false;
    while !sorted {
        sorted = true;
        for rule in rules.iter() {
            if !rule_applies(rule, update) {
                let first_index = update.iter().position(|i| *i == rule.0).unwrap();
                let second_index = update.iter().position(|i| *i == rule.1).unwrap();
                update.swap(first_index, second_index);
                sorted = false;
                break;
            }
        }
    }
}

fn solve(rules: &Vec<Rule>, updates: &mut Vec<Update>) -> i32 {
    updates
        .iter_mut()
        .filter(|update| !is_update_ordered(rules, update))
        .map(|mut update| {
            sort_update(rules, &mut update);
            update
            })
        .map(|update| get_middle(update).unwrap())
        .sum()
}

fn main() {
    let filepath = Path::new("../../inputs/input-05.txt");
    let input = fs::read_to_string(filepath).unwrap();
    let (rules, mut updates) = parse(&input);
    let solution = solve(&rules, &mut updates);
    println!("solution: {}", solution);
}
