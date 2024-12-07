use std::env;
use std::fs::File;
use std::io::{self, BufRead};

fn main() -> io::Result<()> {
    env::set_var("RUST_BACKTRACE", "1");
    let file = File::open("input.txt")?;
    let reader = io::BufReader::new(file);

    let mut col1: Vec<u64> = Vec::new();
    let mut col2: Vec<u64> = Vec::new();

    for line in reader.lines() {
        let line = line?;
        let numbers: Vec<u64> = line
            .split_whitespace()
            .filter_map(|s| s.parse().ok())
            .collect();

        if numbers.len() == 2 {
            col1.push(numbers[0]);
            col2.push(numbers[1]);
        }
    }

    col1.sort();
    col2.sort();
    let mut sum: u64 = 0;

    for i in 0..col1.len() {
        let instances: u64 = col2.iter().filter(|x: &&u64| **x == col1[i]).count() as u64;
        sum += col1[i] * instances;
    }
    println!("{}", sum);
    Ok(())
}
