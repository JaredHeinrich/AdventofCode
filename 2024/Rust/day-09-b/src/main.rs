use std::{fs, path::Path, u64, usize};

#[derive(Debug)]
struct FileSystem {
    files: Vec<File>,
}

impl FileSystem {
    fn from_str(input: &str) -> Self {
        let mut input_iter = input
            .trim()
            .chars()
            .map(|c| c.to_digit(10).unwrap() as usize)
            .enumerate();
        let mut current_index = 0;
        let mut data_drive = Self::new();
        while let Some((i, block_lenght)) = input_iter.next() {
            if i % 2 == 0 {
                let file_id = i / 2;
                data_drive.files.push(File {
                    id: file_id,
                    position: current_index,
                    size: block_lenght,
                });
            }
            current_index += block_lenght;
        }
        data_drive
    }

    fn new() -> Self {
        Self { files: Vec::new() }
    }

    fn calculate_checksum(&self) -> u64 {
        self.files
            .iter()
            .map(|file| file.calculate_checksum())
            .sum()
    }

    fn find_free_space_for_file(&self, file: &File) -> Option<(usize, usize)> {
        for (first_file_index, file_pair) in self.files.windows(2).enumerate() {
            let (first_file, second_file) = (&file_pair[0], &file_pair[1]);
            if first_file.position >= file.position {
                break;
            }
            let free_space_position = first_file.position + first_file.size;
            let free_space_size = second_file.position - free_space_position;
            if free_space_size >= file.size {
                return Some((first_file_index + 1, free_space_position));
            }
        }
        None
    }

    fn get_file_index(&self, file_id: usize) -> Option<usize> {
        self.files.iter().position(|file| file.id == file_id)
    }

    fn move_file(&mut self, source_index: usize, dest_index: usize) {
        let files = &mut self.files;
        if source_index == dest_index || source_index >= files.len() || dest_index >= files.len() {
            return;
        }
        let file = files.remove(source_index);
        let dest_index = if source_index < dest_index {
            dest_index - 1
        } else {
            dest_index
        };
        files.insert(dest_index, file);
    }

    fn defragment(&mut self) {
        let mut current_file_id = self.files.len() - 1;
        loop {
            let current_file_index = self.get_file_index(current_file_id).unwrap();
            if let Some((free_space_index, free_space_position)) =
                self.find_free_space_for_file(self.files.get(current_file_index).unwrap())
            {
                self.files.get_mut(current_file_index).unwrap().position = free_space_position;
                self.move_file(current_file_index, free_space_index);
            }

            if current_file_id <= 0 {
                break;
            }
            current_file_id -= 1;
        }
    }
}

#[derive(Debug)]
struct File {
    id: usize,
    position: usize,
    size: usize,
}
impl File {
    fn calculate_checksum(&self) -> u64 {
        ((self.position..(self.position + self.size))
            .into_iter()
            .sum::<usize>()
            * self.id) as u64
    }
}

fn solve(filepath: &Path) -> u64 {
    let input = fs::read_to_string(filepath).unwrap();
    let mut file_system = FileSystem::from_str(&input);
    file_system.defragment();
    file_system.calculate_checksum()
}

fn main() {
    let filepath = Path::new("../../inputs/input-09.txt");
    let solution = solve(filepath);
    println!("Solution: {solution}");
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_solution() {
        let filepath = Path::new("../../testinputs/testinput-09.txt");
        let test_solution = solve(filepath);
        assert_eq!(test_solution, 2858);
    }
}
