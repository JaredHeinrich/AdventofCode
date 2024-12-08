use std::{fs, path::Path};

type Number = u64;

struct Equation {
    result: Number,
    parameters: Vec<Number>
}

impl Equation {
    fn from_str(str: &str) -> Self {
        let mut str_split = str.split(": ");
        let result = str_split
            .next()
            .unwrap()
            .parse()
            .unwrap();
        let parameters = str_split
            .next()
            .unwrap()
            .split(" ")
            .map(|s| s.parse().unwrap())
            .collect();
        Self{result,parameters}
    }

    fn has_solution(&self) -> bool {
        Self::check_recursiv(self.result, &self.parameters[1..], *self.parameters.first().unwrap())
    }

    fn check_recursiv(final_result: Number, parameters_left: &[Number], current_result: Number) -> bool {
        match parameters_left.first() {
            _ if current_result > final_result => false,
            Some(param) => {
                Self::check_recursiv(final_result, &parameters_left[1..], current_result + param) ||
                Self::check_recursiv(final_result, &parameters_left[1..], current_result * param) ||
                Self::check_recursiv(final_result, &parameters_left[1..], concatenate(current_result, *param))
            },
            None => final_result == current_result,
        }
    }
}

fn concatenate(x: Number, y: Number) -> Number {
    y + x * 10_u64.pow(y.to_string().len() as u32)
}

fn solve(equations: Vec<Equation>) -> Number {
    equations
        .iter()
        .filter(|e| e.has_solution())
        .map(|e| e.result)
        .sum()
}

fn parse(input: &str) -> Vec<Equation> {
    input
        .lines()
        .map(|line| Equation::from_str(line))
        .collect()
}

fn main() {
    let filepath = Path::new("../../inputs/input-07.txt");
    let input: String = fs::read_to_string(filepath).unwrap();
    let equations = parse(&input);
    let solution = solve(equations);
    println!("solution: {}", solution);
}
