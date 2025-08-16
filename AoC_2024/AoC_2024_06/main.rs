////////////////////////////////////////////////////
//              Advent of Code 2024               //
//             Day 6: Guard Gallivant             //
//            Solution by Lorin Lange             //
////////////////////////////////////////////////////

use std::collections::HashSet;
use std::fs;

const FILE_NAME: &str = "input.txt";
const DIRS: [(i32, i32); 4] = [(-1, 0), (0, 1), (1, 0), (0, -1)];

fn parse(path: &str) -> Vec<Vec<char>> {
    let contents = fs::read_to_string(path)
        .expect("Problem while opening the file.");
    let grid: Vec<Vec<char>> = contents
        .lines()
        .filter(|line| !line.is_empty())
        .map(|line| line.chars().collect())
        .collect();
    grid
}

fn find_start(grid: &mut Vec<Vec<char>>) -> (usize, usize) {
    for r in 0..grid.len() {
        for c in 0..grid[r].len() {
            if grid[r][c] == '^' {
                grid[r][c] = '.';
                return (r, c);
            }
        }
    }
    (usize::MIN, usize::MIN)
}

fn part_one(mut pos: (usize, usize), grid: &Vec<Vec<char>>) -> usize {
    let height = grid.len() as i32;
    let width = grid[0].len() as i32;
    let mut dir = 0;
    let mut visited: HashSet<(usize, usize)> = HashSet::new();
    loop {
        visited.insert(pos);
        let r = pos.0 as i32 + DIRS[dir].0;
        let c = pos.1 as i32 + DIRS[dir].1;
        if r < 0 || r >= height || c < 0 || c >= width { break; }
        if grid[r as usize][c as usize] == '.' {
            pos = (r as usize, c as usize);
        } else {
            dir = (dir + 1) % 4;
        }
    }
    visited.len()
}

fn is_loop(mut pos: (usize, usize), grid: &Vec<Vec<char>>) -> bool {
    let height = grid.len() as i32;
    let width = grid[0].len() as i32;
    let mut dir = 0usize;
    let mut visited = vec![false; grid.len() * grid[0].len() * 4];
    loop {
        let hash = dir + (pos.0 * grid[0].len() + pos.1) * 4;
        if visited[hash] { return true; }
        visited[hash] = true;
        let r = pos.0 as i32 + DIRS[dir].0;
        let c = pos.1 as i32 + DIRS[dir].1;
        if r < 0 || r >= height || c < 0 || c >= width {
            return false;
        }
        if grid[r as usize][c as usize] == '.' {
            pos = (r as usize, c as usize);
        } else {
            dir = (dir + 1) % 4;
        }
    }
}

fn part_two(pos: (usize, usize), grid: &mut Vec<Vec<char>>) -> usize {
    let mut counter = 0;
    for r in 0..grid.len() {
        for c in 0..grid[r].len() {
            if grid[r][c] == '.' && (r, c) != pos {
                grid[r][c] = '#';
                if is_loop(pos, grid) {
                    counter += 1;
                }
                grid[r][c] = '.';
            }
        }
    }
    counter
}

fn main() {
    let mut grid = parse(FILE_NAME);
    let start_pos = find_start(&mut grid);
    println!("Part 1: {}", part_one(start_pos, &grid));
    println!("Part 2: {}", part_two(start_pos, &mut grid));
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_input() {
        let mut grid = parse("test_input.txt");
        let start_pos = find_start(&mut grid);
        assert_eq!(part_one(start_pos, &grid), 41);
        assert_eq!(part_two(start_pos, &mut grid), 6);
    }

    #[test]
    fn input() {
        let mut grid = parse("input.txt");
        let start_pos = find_start(&mut grid);
        assert_eq!(part_one(start_pos, &grid), 5086);
        assert_eq!(part_two(start_pos, &mut grid), 1770);
    }

}
