use std::{fs, path::Path};

type FileData = Vec<Option<u64>>;
fn calculate_checksum(file: FileData) -> u64 {
    let mut pointer_front = 0;
    let mut pointer_back = file.len() - 1;
    let mut checksum = 0;
    while pointer_front <= pointer_back {
        let front: &Option<u64> = file.get(pointer_front).unwrap();
        if front.is_some() {
            checksum += front.unwrap() * pointer_front as u64;
            pointer_front += 1;
            continue;
        }
        while file.get(pointer_back).unwrap().is_none() {
            pointer_back -= 1;
        }
        if pointer_front > pointer_back {
            break;
        }
        checksum += file.get(pointer_back).unwrap().unwrap() * pointer_front as u64;
        pointer_back -= 1;
        pointer_front += 1;
    }
    checksum
}
fn parse(input: &str) -> FileData {
    input
        .trim()
        .chars()
        .map(|c| c.to_digit(10).unwrap())
        .enumerate()
        .flat_map(|(i, block_lenght)| {
            let block_lenght = block_lenght as usize;
            if i % 2 == 0 {
                let file_number = i as u64 / 2;
                return vec![Some(file_number); block_lenght];
            }
            return vec![None; block_lenght];
        })
        .collect()
}

fn main() {
    let filepath = Path::new("../../inputs/input-09.txt");
    let input = fs::read_to_string(filepath).unwrap();
    let file_data = parse(&input);
    let solution = calculate_checksum(file_data);
    println!("Solution: {}", solution);
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_solution() {
        let filepath = Path::new("../../testinputs/testinput-09.txt");
        let input = fs::read_to_string(filepath).unwrap();
        let file_data = parse(&input);
        let solution = calculate_checksum(file_data);
        assert_eq!(solution, 1928);
    }
}
