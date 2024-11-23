use std::fs;
fn main() {
    const NUMBERS: [(&str,&str);9] = [
        ("one", "1"),
        ("two", "2"),
        ("three", "3"),
        ("four", "4"),
        ("five", "5"),
        ("six", "6"),
        ("seven", "7"),
        ("eight", "8"),
        ("nine", "9"),
    ];
    let mut input = match fs::read_to_string("../../inputs/input-01.txt") {
        Ok(value) => value,
        Err(_) => String::from("error"),
    };

    for (string, digit) in NUMBERS.iter() {
        let s = format!("{}{}{}",string,digit,string); 
        input = input.replace(string, &s);
    }

    let mut counter = 0; 
    for (index, line) in input.lines().enumerate() {
        let value = get_value(line);
        println!("{}: {}", index, value);
        counter += value;
    }
    println!("{}", counter);
}
fn get_value(string: &str) -> u32 {
    let mut first_digit: u32 = 0;
    let mut last_digit: u32 = 0;
    for c in string.chars() {
        if c.is_digit(10) {
            first_digit = c.to_digit(10).unwrap();
            break;
        }
    }
    for c in string.chars().rev() {
        if c.is_digit(10) {
            last_digit = c.to_digit(10).unwrap();
            break;
        }
    }
    first_digit * 10 + last_digit
}
