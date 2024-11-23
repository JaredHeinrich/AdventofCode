use std::{collections::HashMap, fs, usize};

use num::integer::lcm;

fn main() {
    let input: String = fs::read_to_string("../../inputs/input-08.txt").unwrap();
    let path_finder = PathFinder::from_str(&input);
    let path_lenght = path_finder.get_path_len_fast();
    println!("lenght: {}", path_lenght);
    //path_finder.test_fn();
}

#[allow(dead_code)]
struct PathFinder {
    instructions: Vec<Direction>,
    graph: Graph,
}

#[allow(dead_code)]
impl PathFinder{

    fn test_fn(&self) {
        let mut start_nodes: Vec<&NodeId> = Vec::new();
        for node in self.graph.keys(){
            if node.is_start_node() {
                start_nodes.push(node)
            }
        }
        let mut current_node = (0,start_nodes[5]);
        let mut counter = 1;
        loop {
            current_node = self.find_next_end_node(current_node.0, current_node.1);
            println!("{:?}", current_node.0 / counter);
            counter += 1;
        }
    }

    fn get_path_len_fast(&self) -> usize{
        let mut start_nodes: Vec<&NodeId> = Vec::new();
        for node in self.graph.keys(){
            if node.is_start_node() {
                start_nodes.push(node)
            }
        }
        let number_of_start_nodes = start_nodes.len();
        if number_of_start_nodes <= 0 {return 0;}
        let end_indices: Vec<usize> = start_nodes.iter().map(|node| self.find_next_end_node(0, node).0).collect();
        let mut least_common_multiple = 1;
        for i in end_indices.iter() {
            least_common_multiple = lcm(*i,least_common_multiple);
        }
        for (i,node) in start_nodes.iter().enumerate(){
            let end_node = self.go_n_steps(0, end_indices[i], node);
            println!("{:?}", end_node);
        }
        return least_common_multiple;
    }

    fn get_path_len(&self) -> usize{
        let mut start_nodes: Vec<&NodeId> = Vec::new();
        for node in self.graph.keys(){
            if node.is_start_node() {
                start_nodes.push(node)
            }
        }
        let number_of_start_nodes = start_nodes.len();
        if number_of_start_nodes <= 0 {return 0;}
        let mut current_nodes: Vec<(usize, &NodeId)> = start_nodes.iter().map(|node| (0, *node)).collect();
        let mut index = 0; 
        while index < number_of_start_nodes{
            if index == 0 {
                current_nodes[index] = self.find_next_end_node(current_nodes[index].0, current_nodes[index].1);
            }
            else {
                let first_counter = current_nodes[0].0;
                let my_counter = current_nodes[index].0;
                current_nodes[index] = self.go_n_steps(my_counter, first_counter-my_counter, current_nodes[index].1);
                if !current_nodes[index].1.is_end_node() {
                    index = 0;
                    continue;
                }
            }
            println!("{:?}", current_nodes);
            index += 1;
        }
        current_nodes[0].0
    }

    fn go_n_steps<'a>(&'a self, index: usize, steps: usize, current_node_id: &'a NodeId) -> (usize, &'a NodeId){
        let mut current_node_id = current_node_id;
        let mut index = index;
        for _ in 0..steps {
            current_node_id = self.go_one_step(index, current_node_id);
            index += 1;
        }
        (index, current_node_id)

    }

    fn go_one_step<'a>(&'a self, index: usize, current_node_id: &'a NodeId) -> &'a NodeId{
        let instruction = self.instructions.get(index%self.instructions.len()).unwrap();
        match instruction {
            Direction::Rigth => &self.graph.get(current_node_id).unwrap().next_right,
            Direction::Left => &self.graph.get(current_node_id).unwrap().next_left,
        }
    }

    fn find_next_end_node<'a>(&'a self, index: usize, current_node_id: &'a NodeId) -> (usize, &'a NodeId){
        let mut current_node_id = current_node_id;
        let mut index = index;
        loop {
            current_node_id = self.go_one_step(index, current_node_id);
            index += 1;
            if current_node_id.is_end_node() {
                break;
            }
        }
        (index, current_node_id)
    }


    fn from_str(str: &str) -> Self{
        let mut input_lines = str.lines();
        let instructions = input_lines.next().unwrap().chars().map(|c| Direction::from_char(c).unwrap()).collect();
        input_lines.next();
        let graph = input_lines.map(|line| {
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
        Self{
            instructions,
            graph
        }
    }
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
trait NodeIdTrait {
    fn is_start_node(&self) -> bool;
    fn is_end_node(&self) -> bool;
}
impl NodeIdTrait for NodeId{
    fn is_start_node(&self) -> bool {
        self[self.len()-1] == 'A'
    }
    fn is_end_node(&self) -> bool {
        self[self.len()-1] == 'Z'
    }
}
