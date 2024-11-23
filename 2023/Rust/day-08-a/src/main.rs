use std::{collections::HashMap, fs};

fn main() {
    let start_node_id = ['A','A','A'];
    let end_node_id = ['Z','Z','Z'];
    let instructions: Vec<Direction>;
    let graph: Graph;
    let input: String = fs::read_to_string("../../inputs/input-08.txt").unwrap();
    let mut input_lines = input.lines();
    instructions = input_lines.next().unwrap().chars().map(|c| Direction::from_char(c).unwrap()).collect();
    input_lines.next();
    graph = input_lines.map(|line| {
        let mut line_split = line.split(" = ");
        let node_id: NodeId = line_split.next().unwrap().chars().collect::<Vec<char>>().try_into().unwrap();
        let successors = line_split.next().unwrap();
        let successors = successors.replace(&['(', ')'][..], "");
        let mut successors = successors.split(", ");
        let node_data: NodeData = NodeData{
            next_left: successors.next().unwrap().chars().collect::<Vec<char>>().try_into().unwrap(),
            next_right: successors.next().unwrap().chars().collect::<Vec<char>>().try_into().unwrap(),
        };
        (node_id, node_data)
    }).collect();
    let mut current_node_id: [char;3] = start_node_id;
    let mut counter = 0;
    while current_node_id != end_node_id {
        print!("{} -> ", current_node_id.iter().collect::<String>());
        let instruction: &Direction = &instructions[counter % instructions.len()];
        let current_node: &NodeData = graph.get(&current_node_id).unwrap();
        match instruction {
            Direction::Rigth => current_node_id = current_node.next_right,
            Direction::Left => current_node_id = current_node.next_left,
        }
        counter+=1;
    }
    println!("{}", current_node_id.iter().collect::<String>());
    println!("counter = {}", counter);
}
enum Direction{
    Left,
    Rigth,
}
impl Direction {
    fn from_char(c: char) -> Option<Direction>{
        match c {
            'L' => Some(Direction::Left),
            'l' => Some(Direction::Left),
            'R' => Some(Direction::Rigth),
            'r' => Some(Direction::Rigth),
            _ => None
        }
    }
}
type NodeId = [char;3];
struct NodeData{
    next_left: NodeId,
    next_right: NodeId,
}
type Graph = HashMap<NodeId, NodeData>;
