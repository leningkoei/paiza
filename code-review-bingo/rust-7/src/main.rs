// fn main() {
//     std::io::stdin()
//         .lines()
//         .next()
//         .expect("Error when Reading The First Line")
//         .expect("Error when Reading The First Line");
//
//     let mut input = String::new();
//     std::io::stdin()
//         .read_line(&mut input)
//         .expect("Error when Reading The Second Line");
//     let mut s: Vec<char> = input.trim().chars().collect();
//     s.sort();
//
//     let mut input = String::new();
//     std::io::stdin()
//         .read_line(&mut input)
//         .expect("Error when Reading The Second Line");
//     let mut t: Vec<char> = input.trim().chars().collect();
//     t.sort();
//
//     println!("{}", if s == t { "Yes" } else { "No" });
// }

fn main() {
    let mut lines = std::io::stdin().lines();

    let _n = lines.next();

    let s = lines
        .next()
        .expect("Error when Reading The First Line")
        .expect("Error when Reading The First Line")
        .trim()
        .chars()
        .collect::<Vec<char>>()
        .sorted();

    let t = lines
        .next()
        .expect("Error when Reading The Second Line")
        .expect("Error when Reading The Second Line")
        .trim()
        .chars()
        .collect::<Vec<char>>()
        .sorted();

    println!("{}", if s == t { "Yes" } else { "No" });
}

trait Sortedable {
    fn sorted(self) -> Self
    where
        Self: IntoIterator,
        Self: FromIterator<Self::Item>,
        Self::Item: Ord,
    {
        let mut to_sort: Vec<Self::Item> = self.into_iter().collect();
        to_sort.sort();
        to_sort.into_iter().collect()
    }
}

impl<T> Sortedable for Vec<T> {}
