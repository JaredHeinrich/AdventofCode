use std::{fs, isize, process::exit, u32, usize};
fn main() {
    let calc = Calculator::new("../input.txt");
    println!("result: {}", calc.calculate());
}

pub enum Direction{
    Right,
    Left,
    Both,
}

pub struct Calculator{
    chars: Vec<char>,
    side_len: usize,
    size: usize,
}

impl Calculator{
    pub fn new(path: &str) -> Self{
        let input = match fs::read_to_string(path)
        {
            Ok(v) => v,
            Err(_) => {
                println!("fehler beim lesen");
                exit(0);
            }
        };
        let input = input.trim();
        let side_len = input.lines().count();
        let chars: Vec<char> = input.chars().filter(|char| *char != '\n').collect();
        let size = chars.len();
        Self{
            side_len,
            chars,
            size
        }
    }

    pub fn calculate(&self) -> u32{
        let mut result = 0;
        for (i, char) in self.chars.iter().enumerate() {
            if char.is_digit(10)||*char=='.' {
                continue;
            }
            result += self.handle_symbol(i);
        }
        result
    }

    fn handle_symbol(&self, index: usize) -> u32{
        fn check_right_of(calc: &Calculator, numbers: &mut Vec<u32>, i: isize){
            match calc.handle_field(i+1, Direction::Right){
                0 => {},
                x => {
                    numbers.push(x);
                },
            }
        }

        fn check_left_of(calc: &Calculator, numbers: &mut Vec<u32>, i: isize){
            match calc.handle_field(i-1, Direction::Left){
                0 => {},
                x => {
                    numbers.push(x);
                },
            }
        }

        fn check_left_and_right_of(calc: &Calculator, numbers: &mut Vec<u32>, i: isize){
            match calc.handle_field(i, Direction::Both) {
                0 => {
                    check_right_of(calc, numbers, i);
                    check_left_of(calc, numbers, i);
                },
                x => {
                    numbers.push(x);
                }
            };
        }

        let mut numbers: Vec<u32> = Vec::new();
        let mut i;
        i = index as isize - self.side_len as isize;
        check_left_and_right_of(self, &mut numbers, i);
        i = index as isize + self.side_len as isize;
        check_left_and_right_of(self, &mut numbers, i);
        i = index as isize;
        check_right_of(self, &mut numbers, i);
        check_left_of(self, &mut numbers, i);
        if numbers.len() != 2 {
            return 0;
        }
        numbers[0]*numbers[1]
    }

    fn handle_field(&self, index: isize, check_direction: Direction) -> u32 {

        fn is_on_right(i: isize, side: usize) -> bool{
            (i + side as isize) % side as isize == side as isize - 1
        }
        fn is_on_left(i: isize, side: usize) -> bool{
            (i + side as isize) % side as isize == 0
        }
        fn handle_right(i: isize, calc: &Calculator, res: &mut String){
            let mut tmp_index = i;
            let mut c: char;
            while !is_on_left(tmp_index, calc.side_len){
                c = calc.chars[tmp_index as usize];
                if !c.is_digit(10){
                    break;
                }
                res.push(c);
                tmp_index+=1;
            }
        }
        fn handle_left(i: isize, calc: &Calculator, res: &mut String){
            let mut tmp_index = i;
            let mut c: char;
            while !is_on_right(tmp_index, calc.side_len){
                c = calc.chars[tmp_index as usize];
                if !c.is_digit(10){
                    break;
                }
                res.insert(0,c);
                tmp_index-=1;
            }
        }
        if index < 0 || index >= self.size as isize{
            return 0;
        }

        let result: String = match check_direction{
            Direction::Both => {
                let mut result = String::new();
                let c = self.chars[index as usize];
                if c.is_digit(10) {
                    result.push(c);
                    handle_right(index+1, self, &mut result);
                    handle_left(index-1, self, &mut result);
                }
                result
            },
            Direction::Right => {
                let mut result = String::new();
                handle_right(index, self, &mut result);
                result
            },
            Direction::Left => {
                let mut result = String::new();
                handle_left(index, self, &mut result);
                result
            },
        };
        match result.parse::<u32>() {
            Ok(n) => n,
            Err(_) => 0,
        }
    }
}
