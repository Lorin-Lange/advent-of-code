////////////////////////////////////////////////////
//              Advent of Code 2024               //
//           Day 1: Historian Hysteria            //
//            Solution by Lorin Lange             //
////////////////////////////////////////////////////

use std::fs;
use std::collections::HashMap;

fn parse(path : &str) -> (Vec<i32>, Vec<i32>) {
    let input = fs::read_to_string(path).expect("Couldn't read file.");
    let lines = input.lines().collect::<Vec<_>>();
    let (mut l1, mut l2) = (Vec::new(), Vec::new());
    for line in lines {
        let mut st = line.split_whitespace();
        let mut n = st.next().unwrap().parse().unwrap();
        l1.push(n);
        n = st.next().unwrap().parse().unwrap();
        l2.push(n);
    }
    l1.sort();
    l2.sort();
    (l1, l2)
}

fn part_1(l1 : &Vec<i32>, l2 : &Vec<i32>) -> i32 {
    l1.iter().zip(l2).map(|(x, y)| (x - y).abs()).sum()
}

fn part_2(l1 : &Vec<i32>, l2 : &Vec<i32>) -> i32 {
    let mut map = HashMap::new();
    for &x in l2 {
        *map.entry(x).or_insert(0) += 1;
    }
    l1.iter().map(|&a| a * map.get(&a).unwrap_or(&0)).sum()
}

#[test]
fn test_input() {
    let (lst1, lst2) = parse("test_input.txt");
    assert_eq!(part_1(&lst1, &lst2), 11);
    assert_eq!(part_2(&lst1, &lst2), 31);
}

#[test]
fn input() {
    let (lst1, lst2) = parse("input.txt");
    assert_eq!(part_1(&lst1, &lst2), 2430334);
    assert_eq!(part_2(&lst1, &lst2), 28786472);
}

fn main() {
    let (lst1, lst2) = parse("input.txt");
    println!("Part 1: {}", part_1(&lst1, &lst2));
    println!("Part 2: {}", part_2(&lst1, &lst2));
}
