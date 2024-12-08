use std::{fs, i32, path::Path, usize};

#[derive(Clone, Copy, PartialEq, Debug)]
enum Field {
    Empty,
    Block,
    Path
}

impl Field {
    fn from_char(c: char) -> Self {
        match c {
            '#' => Field::Block,
            'X' => Field::Path,
            _ => Field::Empty
        }
    }
}

enum Direction {
    Up,
    Right,
    Down,
    Left
}

impl Direction {
    fn to_vector(&self) -> Vector {
        match self {
            Direction::Up => Vector::from_touple((0,-1)),
            Direction::Right => Vector::from_touple((1,0)),
            Direction::Down => Vector::from_touple((0,1)),
            Direction::Left => Vector::from_touple((-1,0))
        }
    }

    fn turn_right(&mut self) {
        *self = match self{
            Direction::Up => Direction::Right,
            Direction::Right => Direction::Down,
            Direction::Down => Direction::Left,
            Direction::Left => Direction::Up,
        }
    }
}

#[derive(Clone, Copy, Debug)]
struct Vector {
    x: i32,
    y: i32
}

impl Vector {
    fn from_touple(touple: (i32, i32)) -> Self {
        Self{x: touple.0, y: touple.1}
    }
    fn from_index(index: i32, grid_size: i32) -> Self {
        let x = index % grid_size;
        let y = index / grid_size;
        Self{x,y}
    }
    fn to_index(&self, grid_size: i32) -> i32 {
        self.x + self.y * grid_size
    }
    fn add(&self, other: &Self) -> Self {
        let x = self.x + other.x;
        let y = self.y + other.y;
        Self{x,y}
    }
}

struct Board {
    size: i32,
    fields: Vec<Field>,
    guard: Guard
}

impl Board {
    fn from_str(board_str: &str) -> Self {
        let size = board_str.lines().count() as i32;
        let fields = board_str
            .chars()
            .filter(|c| *c != '\n')
            .map(|c| Field::from_char(c))
            .collect();
        let guard_index = board_str
            .chars()
            .filter(|c| *c != '\n')
            .position(|c| c == '^')
            .unwrap() as i32;
        let guard = Guard::new(Vector::from_index(guard_index, size));
        Board {size, fields, guard}
    }

    fn move_guard_to_end(&mut self) {
        while self.is_vector_inside(&self.guard.position) {
            match self.get_field_by_vector(&self.guard.position_ahead()) {
                Some(field) if (field == Field::Block) => {
                    self.guard.turn_right();
                },
                _ => {
                    let guard_position = self.guard.position;
                    self.set_field_by_vector(&guard_position, Field::Path).unwrap();
                    self.guard.move_forward();
                }
            }
        }
    }

    fn set_field_by_vector(&mut self, vector: &Vector, field: Field) -> Result<(),()> {
        if !self.is_vector_inside(vector) {
            return Err(());
        }
        let f = self.fields.get_mut(vector.to_index(self.size) as usize).unwrap();
        *f = field;
        Ok(())
    }
    
    fn get_field_by_vector(&self, vector: &Vector) -> Option<Field> {
        if !self.is_vector_inside(vector) {
            return None;
        }
        self.fields.get(vector.to_index(self.size) as usize).copied()
    }

    fn is_vector_inside(&self, vector: &Vector) -> bool {
        if vector.x < 0 || vector.y < 0 {
            return false;
        }
        if vector.x >= self.size || vector.y >= self.size {
            return false;
        }
        true
    }
    fn count_path_lenght(&self) -> i32 {
        self.fields
            .iter()
            .filter(|f| **f == Field::Path)
            .count() as i32
    }
}

struct Guard {
    direction: Direction,
    position: Vector
}

impl Guard {
    fn new(position: Vector) -> Self {
        Self{direction: Direction::Up, position}
    }
    fn position_ahead(&self) -> Vector {
        self.position.add(&self.direction.to_vector())
    }
    fn move_forward(&mut self) {
        self.position = self.position.add(&self.direction.to_vector())
    }
    fn turn_right(&mut self) {
        self.direction.turn_right();
    }

}

fn solve(mut board: Board) -> i32 {
    board.move_guard_to_end();
    board.count_path_lenght()
}

fn main() {
    let filepath = Path::new("../../inputs/input-06.txt");
    let input: String = fs::read_to_string(filepath).unwrap();
    let board = Board::from_str(&input);
    let solution = solve(board);
    println!("solution: {}", solution);
}
