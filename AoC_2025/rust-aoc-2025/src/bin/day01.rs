
use rust_aoc_2025::*;

fn part_1() {
    println!("{}", test_fun())
}

fn part_2() {
    println!("Part 2 of day 1.")
}

fn main() {
    part_1();
    part_2();
    let input = include_str!("../../inputs/day01/input.txt");
    let test_input = include_str!("../../inputs/day01/test_input.txt");
    println!("Part test 1: {}", test_input);
    println!("Part 1: {}", input);
}

#[cfg(test)]
mod tests_day01 {
    use super::*;

    #[test]
    fn test_part_1() {
        part_1();
        assert_eq!(true, true);
    }

    #[test]
    fn test_part_2() {
        part_2();
    }

}
