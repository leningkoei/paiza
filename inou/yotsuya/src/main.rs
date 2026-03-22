use std::{convert::TryInto, io::BufRead};

fn main() {
    let [n]: [usize; 1] = std::io::stdin()
        .lock()
        .lines()
        .map(|line| line.expect("IO Error: Reading from stdin"))
        .map(|line| line.trim().to_string())
        .filter(|line| !line.is_empty())
        .map(|line| {
            line.parse()
                .expect("Input Error: Expect an usize at the first line")
        })
        .collect::<Vec<_>>()
        .try_into()
        .expect("Input Error: Expect at least one line");

    println!("{}", n * 100);
}
