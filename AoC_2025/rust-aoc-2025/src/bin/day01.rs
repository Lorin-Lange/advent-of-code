////////////////////////////////////////////////////
//              Advent of Code 2025               //
//             Day 1: Secret Entrance             //
//            Solution by Lorin Lange             //
////////////////////////////////////////////////////

const INPUT: &str = include_str!("../../inputs/day01/input.txt");
const TEST_INPUT: &str = include_str!("../../inputs/day01/test_input.txt");

fn parse(inp: &str) -> Vec<(i32, i32)> {
    let mut lst = Vec::new();
    for line in inp.lines() {
        let chars: Vec<char> = line.chars().collect();
        let dir = chars[0];
        let dist: i32 = line[1..].parse().unwrap();
        let step = match dir {
            'L' => -1,
            'R' => 1,
            _ => panic!("Invalid format."),
        };
        lst.push((step, dist));
    }
    lst
}

fn part_1(inp: &Vec<(i32, i32)>) -> i32 {
    let mut dial: i32 = 50;
    let mut res: i32 = 0;
    for (step, dist) in inp.iter() {
        for _ in 0..*dist {
            dial += step;
        }
        if dial % 100 == 0 {
            res += 1;
        }
    }
    res
}

fn part_2(inp: &Vec<(i32, i32)>) -> i32 {
    let mut dial: i32 = 50;
    let mut res: i32 = 0;
    for (step, dist) in inp.iter() {
        for _ in 0..*dist {
            dial += step;
            if dial % 100 == 0 {
                res += 1;
            }
        }
    }
    res
}

fn main() {
    let inp = parse(INPUT);
    println!("Part 1: {}", part_1(&inp));
    println!("Part 2: {}", part_2(&inp));
}

#[cfg(test)]
mod tests_day01 {
    use super::*;

    #[test]
    fn test_part_1() {
        assert_eq!(3, part_1(&parse(TEST_INPUT)));
        assert_eq!(1031, part_1(&parse(INPUT)));
    }

    #[test]
    fn test_part_2() {
        assert_eq!(6, part_2(&parse(TEST_INPUT)));
        assert_eq!(5831, part_2(&parse(INPUT)));
    }
}
