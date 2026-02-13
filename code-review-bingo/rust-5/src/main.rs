fn main() {
    let mut input = String::new();
    std::io::stdin()
        .read_line(&mut input)
        .expect("Error when Reading Stdin");
    let n: i32 = input.trim().parse().expect("Error when Parsing N");

    println!("{}", n * (1 + n) / 2);
}
