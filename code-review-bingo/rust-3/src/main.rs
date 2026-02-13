fn main() {
    let mut input = String::new();
    std::io::stdin()
        .read_line(&mut input)
        .expect("Read Stdin Error");
    let mut input_iter = input.trim().split(' ');

    let a: i32 = input_iter
        .next()
        .expect("Input Nothing!")
        .parse()
        .expect("The First Input Cannot Parse to A `i32`!");
    let b: i32 = input_iter
        .next()
        .expect("Input Nothing!")
        .parse()
        .expect("The Second Input Cannot Parse to A `i32`!");

    println!("{}", a + b);
}
