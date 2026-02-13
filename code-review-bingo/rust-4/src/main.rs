fn main() {
    let mut input = String::new();
    std::io::stdin()
        .read_line(&mut input)
        .expect("Error when Reading Stdin");
    let a: i32 = input
        .trim()
        .parse()
        .expect("Error when Parsing The First Number");

    let mut input = String::new();
    std::io::stdin()
        .read_line(&mut input)
        .expect("Error when Reading Stdin");
    let b: i32 = input
        .trim()
        .parse()
        .expect("Error when Parsing The First Number");

    println!("{}", if a >= b { a } else { b });
}
