fn main() {
    let mut input = String::new();
    std::io::stdin()
        .read_line(&mut input)
        .expect("Error when Reading The First Line");
    let a_list: Vec<i32> = convert_str_to_list(&input.trim(), ' ')
        .expect("Error when Convert The First Line to A i32 List");

    let mut input = String::new();
    std::io::stdin()
        .read_line(&mut input)
        .expect("Error when Reading The First Line");
    let b_list: Vec<i32> = convert_str_to_list(&input.trim(), ' ')
        .expect("Error when Convert The Second Line to A i32 List");

    let not_found_list: Vec<bool> = b_list
        .iter()
        .map(|b| a_list.contains(b))
        .filter(|found| found == &false)
        .collect();

    println!(
        "{}",
        if not_found_list.len() == 0 {
            "Yes"
        } else {
            "No"
        }
    )
}

fn convert_str_to_list<T: std::str::FromStr>(
    str: &str,
    split_char: char,
) -> Result<Vec<T>, T::Err> {
    str.split(split_char)
        .map(|a_n_str| a_n_str.parse())
        .collect()
}
