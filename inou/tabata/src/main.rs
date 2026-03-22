use std::{convert::TryInto, io::BufRead};

fn main() {
    let [line1, line2]: [String; 2] = std::io::stdin()
        .lock()
        .lines()
        .map(|line| line.expect("IO Error: Reading from stdin"))
        .map(|line| line.trim().to_string())
        .filter(|line| !line.is_empty())
        .take(2)
        .collect::<Vec<_>>()
        .try_into()
        .expect("Input Error: Expect at least two lines");

    let a: usize = line1
        .parse()
        .expect("Input Error: Expect a number at the first line");
    let b: usize = line2
        .parse()
        .expect("Input Error: Expect a number at the second line");

    println!("{}", if a > b { "Yes" } else { "No" });
}
