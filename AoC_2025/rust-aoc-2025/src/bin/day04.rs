

use rust_aoc_2025::*;

fn part_1() {
    print!("Part 1");
}

fn part_2() {
    print!("Part 1");
}

fn main() {
    part_1();
    part_2();
    let input = include_str!("../../inputs/day04/input.txt");
    println!("Part 1: {}", input);
}

#[cfg(test)]
mod tests_day04 {
    use super::*;

    #[test]
    fn test_part_1() {
        part_1();
        assert_eq!(true, true);
    }

    #[test]
    fn test_part_2() {
        part_2();
        assert_eq!(true, true);
    }

}
