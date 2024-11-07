use std::{char, fs, usize};

fn main() {
    let input = fs::read_to_string("../input.txt").unwrap();
    let grid = Grid::from_str(&input);
    let length = grid.count_path_length();
    println!("result: {}", length.div_ceil(2));
}
struct Grid{
    grid: Vec<Pipe>,
    size: usize,
    start_index: usize,
}

impl Grid {
    fn from_str(input_str: &str) -> Self {
        let grid = input_str.chars().filter(|c|*c!='\n').map(|c|Pipe::from_char(c)).collect();
        let size: usize = input_str.find("\n").unwrap();
        let start_index: usize = input_str.chars().filter(|c|*c!='\n').position(|c|c=='S').unwrap();
        Self{
            grid,
            size,
            start_index,
        }
    }
    fn check_direction(&self, position: usize, direction: Direction) -> bool{
        match direction{
            Direction::Top => {
                if position < self.size {return false;}
                let next_pipe = &self.grid[position-self.size];
                match next_pipe {
                    Pipe::None => return false,
                    Pipe::Start => return true,
                    Pipe::Directional(pipe) => {
                        return pipe.contains(&direction.reverse());
                    }
                }
            },
            Direction::Right => {
                if position % self.size == self.size - 1 {return false;}
                let next_pipe = &self.grid[position+1];
                match next_pipe {
                    Pipe::None => return false,
                    Pipe::Start => return true,
                    Pipe::Directional(pipe) => {
                        return pipe.contains(&direction.reverse());
                    }
                }
            },
            Direction::Bottom => {
                if position >= (self.size-1)*self.size {return false;}
                let next_pipe = &self.grid[position+self.size];
                match next_pipe {
                    Pipe::None => return false,
                    Pipe::Start => return true,
                    Pipe::Directional(pipe) => {
                        return pipe.contains(&direction.reverse());
                    }
                }
            },
            Direction::Left => {
                if position % self.size == 0 {return false;}
                let next_pipe = &self.grid[position-1];
                match next_pipe {
                    Pipe::None => return false,
                    Pipe::Start => return true,
                    Pipe::Directional(pipe) => {
                        return pipe.contains(&direction.reverse());
                    }
                }
            },
        }
    }

    fn take_step(&self, position: usize, direction: Direction) -> Option<(usize, Direction)> {
        let next_pipe_index = match direction{
            Direction::Top => position - self.size,
            Direction::Right =>  position + 1,
            Direction::Bottom => position + self.size,
            Direction::Left => position - 1,
        };
        let next_pipe: &Pipe = &self.grid[next_pipe_index];
        match next_pipe{
            Pipe::None => None,
            Pipe::Start => Some((next_pipe_index, direction)),
            Pipe::Directional(pipe) => {
                let rev_direction = direction.reverse();
                let next_direction;
                if pipe.0 == rev_direction {
                    next_direction = pipe.1;
                }
                else {
                    next_direction = pipe.0;
                }
                Some((next_pipe_index, next_direction))
            },
        }

    }

    fn count_path_length(&self) -> usize{
        let mut current_position = self.start_index;
        let mut current_direction_opt: Option<Direction> = None;
        for dir in Direction::all() {
            if self.check_direction(current_position, dir){
                current_direction_opt = Some(dir);
                break;
            }
        }
        let mut current_direction: Direction;
        match current_direction_opt{
            None => return 0,
            Some(dir) => current_direction = dir,
        }
        let mut counter = 0;
        loop {
            (current_position, current_direction) = self.take_step(current_position, current_direction).unwrap();
            if self.grid[current_position] == Pipe::Start{
                break;
            }
            counter+=1;
        }
        counter
    }
}

trait DirectionalPipe {
    fn contains(&self, direction: &Direction) -> bool;
    fn get_out_direction(&self, in_direction: &Direction) -> Direction;
}

impl DirectionalPipe for (Direction, Direction) {
    fn contains(&self, direction: &Direction) -> bool {
        self.0 == *direction || self.1 == *direction
    }
    fn get_out_direction(&self, in_direction: &Direction) -> Direction {
        let tmp_direction = in_direction.reverse();
        if self.0 == tmp_direction {
            return self.1.clone();
        }
        return self.0.clone();
    }
}


#[derive(PartialEq, Eq)]
enum Pipe{
    Directional((Direction, Direction)),
    Start,
    None
}
impl Pipe {
    fn from_char(c: char) -> Self {
        match c {
            'F' => Pipe::Directional((Direction::Bottom, Direction::Right)),
            'L' => Pipe::Directional((Direction::Top, Direction::Right)),
            'J' => Pipe::Directional((Direction::Top, Direction::Left)),
            '7' => Pipe::Directional((Direction::Bottom, Direction::Left)),
            '-' => Pipe::Directional((Direction::Left, Direction::Right)),
            '|' => Pipe::Directional((Direction::Top, Direction::Bottom)),
            'S' => Pipe::Start,
            _ => Pipe::None,
        }
    }
}
#[derive(PartialEq, Eq, Clone, Copy)]
enum Direction{
    Top,
    Bottom,
    Right,
    Left,
}
impl Direction{
    fn all() -> Vec<Self>{
        vec![Self::Top, Self::Bottom, Self::Right, Self::Left]
    }

    fn reverse(&self) -> Direction{
        match self{
            Self::Top => Self::Bottom,
            Self::Bottom => Self::Top,
            Self::Left => Self::Right,
            Self::Right => Self::Left,
        }
    }
}
