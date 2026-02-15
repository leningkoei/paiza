use std::convert::TryInto;
use std::io::BufRead;

fn main() {
    let [line1, line2, line3]: [String; 3] = std::io::stdin()
        .lock()
        .lines()
        .map(|line| line.expect("IO Error: Reading from stdin"))
        .map(|line| line.trim().to_string())
        .filter(|line| !line.is_empty())
        .take(3)
        .collect::<Vec<_>>()
        .try_into()
        .expect("Input Error: Expected at lest 3 lines");

    let line1: Vec<char> = line1.chars().collect();
    let line2: Vec<char> = line2.chars().collect();
    let line3: Vec<char> = line3.chars().collect();
    let bingo: Vec<Vec<char>> = vec![line1, line2, line3];

    let mut result = 0;
    // row
    result += (0..3)
        .filter(|&i| bingo[i][0] == bingo[i][1] && bingo[i][1] == bingo[i][2] && bingo[i][2] == '#')
        .count();
    // column
    result += (0..3)
        .filter(|&i| bingo[0][i] == bingo[1][i] && bingo[1][i] == bingo[2][i] && bingo[2][i] == '#')
        .count();
    // diagonal
    result += ((0..3).map(|i| bingo[i][i]).all_equal() && bingo[1][1] == '#') as usize;
    // anti-diagonal
    result += ((0..3).map(|i| bingo[i][2 - i]).all_equal() && bingo[1][1] == '#') as usize;

    println!("{}", result);
}

trait AllEqual: Iterator + Sized
where
    Self::Item: PartialEq,
{
    fn all_equal(mut self) -> bool {
        match self.next() {
            None => true,
            Some(val) => self._all_equal_helper(val),
        }
    }
    #[doc(hidden)]
    fn _all_equal_helper(mut self, last_val: Self::Item) -> bool {
        match self.next() {
            None => true,
            Some(val) => val == last_val && self._all_equal_helper(val),
        }
    }
}

impl<I, F, B> AllEqual for std::iter::Map<I, F>
where
    I: Iterator,
    F: FnMut(I::Item) -> B,
    B: PartialEq,
{
}
