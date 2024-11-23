use std::{fs, process::exit, collections::HashMap};
fn main() {
    let input = match fs::read_to_string("../../inputs/input-03.txt")
    {
        Ok(v) => v,
        Err(_) => {
            println!("fehler beim lesen");
            exit(0);
        }
    };
    let input = input.trim();
    let mut result: i32 = 0;
    let mut filtered_numbers: HashMap<(usize, usize), i32> = HashMap::new();
    let lines: Vec<Vec<char>> = input
        .split("\n")
        .map(|line|
              {
                  line.chars().collect()
              }
            )
        .collect();
    let number_of_lines: i32 = lines.len() as i32;
    let number_of_chars: i32 = lines[0].len() as i32;
    if !{lines.iter().all(|line| { line.len() as i32 == number_of_chars})} {
        println!("fehler im input");
        exit(0);
    }
    for (line_index, line) in lines.iter().enumerate() {
        for (char_index, char) in line.iter().enumerate() {
            let is_sign: bool = !(char.is_digit(10)||*char == '.');
            println!("{},{}", char, is_sign);
            if is_sign {
                println!("found sign");
                check_sign(&mut filtered_numbers, &lines, line_index, char_index, number_of_lines, number_of_chars);
            }
        }
    }

    let mut temp_number: i32 = 0;
    println!("{:?}", filtered_numbers);
    for line_index in 0..number_of_lines as usize {
        for char_index in 0..number_of_chars as usize{
            if char_index as i32 == 0 {
                temp_number = 0;
            }
            match filtered_numbers.get(&(line_index, char_index)) {
                Some(x) => {
                    temp_number = temp_number*10 + x;
                },
                None => {
                    if temp_number > 0 {
                        result += temp_number;
                        println!("{}",temp_number);
                        temp_number = 0;
                    }
                },
            };
            if char_index as i32 == number_of_chars - 1 {
                result += temp_number;
                println!("{}",temp_number);
                temp_number = 0;
            }
        }
    }
    println!("{}", result);
}

fn check_sign(filtered_numbers: &mut HashMap<(usize, usize), i32>,
              lines: &Vec<Vec<char>>,
              line_index: usize,
              char_index: usize,
              number_of_lines: i32,
              number_of_chars: i32) {
    let directions: [(i32,i32); 8] = [
        (-1,-1),
        (-1,0),
        (-1,1),
        (0,1),
        (1,1),
        (1,0),
        (1,-1),
        (0,-1),
    ]; 
    for (x_dir, y_dir) in directions.iter() {
        let line = line_index as i32 + y_dir;
        let char = char_index as i32 + x_dir;

        if line >= 0 
            && line < number_of_lines
                && char >= 0 
                && char < number_of_chars
                && lines[line as usize][char as usize].is_digit(10) {
                    add_number(
                        filtered_numbers,
                        lines,
                        line as usize,
                        char as usize,
                        number_of_chars,
                        true,
                        true
                        );
                }
    }
}

fn add_number(filtered_numbers: &mut HashMap<(usize, usize), i32>,
              lines: &Vec<Vec<char>>,
              line_index: usize,
              char_index: usize,
              number_of_chars: i32,
              check_left: bool,
              check_right: bool) {
    let char = lines[line_index][char_index]; 
    if char.is_digit(10) {
        filtered_numbers.insert((line_index, char_index), char.to_digit(10).unwrap() as i32);
        if check_left {
            let new_char: i32 = char_index as i32 - 1;
            if new_char >= 0 {
                add_number(
                    filtered_numbers,
                    lines,
                    line_index,
                    new_char as usize,
                    number_of_chars,
                    true,
                    false
                    );
            }
        }

        if check_right {
            let new_char: i32 = char_index as i32 + 1;
            if new_char < number_of_chars{
                add_number(
                    filtered_numbers,
                    lines,
                    line_index,
                    new_char as usize,
                    number_of_chars,
                    false,
                    true 
                    );
            }

        }
    }
}

