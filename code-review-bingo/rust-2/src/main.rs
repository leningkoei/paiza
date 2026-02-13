fn main() {
    let mut input = String::new();
    std::io::stdin()
        .read_line(&mut input)
        .expect("Errow when reading a string");

    let n: i32 = input
        .trim()
        .parse()
        .expect("Error when parsering a string to i32");

    (0..n).for_each(|_i| println!("BINGO!"));
}
