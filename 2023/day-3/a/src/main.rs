use std::{fs, process::exit};
fn main() {
    let input = match fs::read_to_string("../input.txt")
    {
        Ok(v) => v,
        Err(_) => {
            println!("fehler beim lesen");
            exit(0);
        }
    };
    let input = input.trim();
    let mut result: u32 = 0;
    let chars: Vec<Vec<char>> = input
        .split("\n")
        .map(|line|
              {
                  line.chars().collect()
              }
            )
        .collect();
    let number_rows: usize = chars.len();
    let number_columns: usize = chars[0].len();
    if !{chars.iter().all(|row| { row.len() == number_columns})} {
        println!("fehler im input");
        exit(0);
    }
    for (row_index, line) in chars.iter().enumerate() {
        for (column_index, _char) in line.iter().enumerate() {
            check_char(&mut result, &chars, row_index, column_index, number_rows, number_columns);
        }
    }
}
fn check_char(result: &mut u32, chars: &Vec<Vec<char>>, row: usize, column: usize, number_rows: usize, number_columns: usize) {
    let range_rows = 0..number_rows;
    let range_columns = 0..number_columns;
    if !(range_rows.contains(&row) && range_columns.contains(&column)) {

    }
}
