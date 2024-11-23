use std::fs;

fn main() {
    let input = fs::read_to_string("../../inputs/input-09.txt").unwrap();
    let input: Vec<Vec<i32>> = Vec::from_str(&input);
    let result: i32 = input.into_iter().map(|row| calculate_prediction(row)).collect::<Vec<i32>>().iter().sum();
    println!("{result}")
}

fn calculate_prediction(previous_values: Vec<i32>) -> i32{
    let mut div_vecs: Vec<Vec<i32>> = Vec::new();
    let length = previous_values.len();
    div_vecs.push(previous_values);
    loop {
        let counter = div_vecs.len();
        div_vecs.push(Vec::new());
        let mut all_zeros = true;
        for i in 0..length-counter {
            let div = div_vecs[counter-1][i+1] - div_vecs[counter-1][i];
            div_vecs[counter].push(div);
            if div != 0{
                all_zeros = false;
            }
        }
        if all_zeros {
            break;
        }
    }
    let mut result = 0;
    for div_vec in div_vecs.iter().rev(){
        result = div_vec.first().unwrap() - result;
    }
    result
}

trait OasisTrait {
    fn from_str(input_str: &str) -> Self;
}

impl OasisTrait for Vec<Vec<i32>>{
    fn from_str(input_str: &str) -> Self {
        input_str
            .lines()
            .map(|line|
                line.split_whitespace().map(|number|
                    number.parse::<i32>().unwrap()
                    ).collect()
                ).collect()
    }
}
