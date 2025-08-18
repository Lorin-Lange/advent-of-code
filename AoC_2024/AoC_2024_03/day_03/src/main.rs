////////////////////////////////////////////////////
//              Advent of Code 2024               //
//              Day 3: Mull It Over               //
//            Solution by Lorin Lange             //
////////////////////////////////////////////////////

use std::fs;
use regex::Regex;
use once_cell::sync::Lazy;

fn parse<'a>(path: &str, buffer: &'a mut String) -> std::io::Result<&'a str> {
    *buffer = fs::read_to_string(path)?;
    Ok(buffer.as_str())
}

fn part_1(input: &str) -> u64 {
    static RE: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"mul\((\d{1,3}),(\d{1,3})\)").unwrap()
    });
    RE.captures_iter(input)
        .map(|cap| {
            let (_, [a, b]) = cap.extract();
            a.parse::<u64>().unwrap() * b.parse::<u64>().unwrap()
        })
        .sum()
}

fn part_2(input: &str) -> u64 {
    static RE: Lazy<Regex> = Lazy::new(|| {
        Regex::new(r"mul\((\d{1,3}),(\d{1,3})\)|do\(\)|don't\(\)").unwrap()
    });
    let mut sum = 0;
    let mut add_up = true;
    for cap in RE.captures_iter(input) {
        match cap.get(0).map(|m| m.as_str()).unwrap() {
            "do()" => add_up = true,
            "don't()" => add_up = false,
            _ if add_up => {
                let a = cap[1].parse::<u64>().unwrap();
                let b = cap[2].parse::<u64>().unwrap();
                sum += a * b;
            }
            _ => {}
        }
    }
    sum
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let mut buf = String::new();
    let input = parse("../input.txt", &mut buf)?;
    println!("Part 1: {}", part_1(&input));
    println!("Part 2: {}", part_2(&input));
    Ok(())
}

#[cfg(test)]
mod test_03 {
    use super::*;

    #[test]
    fn test_input_03_1() {
        let test_input_1 = "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))";
        assert_eq!(part_1(&test_input_1), 161);
    }

    #[test]
    fn test_input_03_2() {
        let test_input_2 = "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))";
        assert_eq!(part_2(&test_input_2), 48);
    }

    #[test]
    fn input_03_1() -> Result<(), Box<dyn std::error::Error>> {
        let mut buf = String::new();
        let content = parse("../input.txt", &mut buf)?;
        assert_eq!(part_1(&content), 173785482);
        Ok(())
    }

    #[test]
    fn input_03_2() -> Result<(), Box<dyn std::error::Error>> {
        let mut buf = String::new();
        let content = parse("../input.txt", &mut buf)?;
        assert_eq!(part_2(&content), 83158140);
        Ok(())
    }

}
