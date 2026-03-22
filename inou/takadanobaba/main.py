"""
```lean
def read : IO $ String × List String := do
  let stdin ← IO.getStdin
  let _n ← stdin.getLine
  let ss ← pure $ (← stdin.getLine)
    |>.trimAscii |>.split ' ' |>.toList |>.map toString
  let head := match ss.head? with
  | .none => panic! "Input Error: Except at least one word in second line."
  | .some head => head
  pure (head, ss.tail)

def eval : String × List String → Bool
| (head, body) =>
  let isProp : String → Bool := λ s ↦ match s.toLower with
  | "at" | "by" | "for" | "from" | "in" | "of" | "on" | "to" => true
  | _ => false
  let checkAllLower : String → Bool := λ s ↦ s.all Char.isLower
  let rec helper : Bool → List String → Bool
  | false, _ => false
  | true, [] => true
  | true, curr :: rest =>
    helper (if isProp curr then checkAllLower curr else curr.front.isUpper) rest
  helper (
    if isProp head
      then checkAllLower (head.drop 1).toString && head.front.isUpper
      else head.front.isUpper
  ) body

def print : Bool → IO Unit
| true => IO.println "Yes"
| false => IO.println "No"

def main : IO Unit := Functor.discard $ print =<< eval <$> read
```
"""

import sys

def read() -> tuple[str, list[str]]:
    stdin = sys.stdin
    _n = stdin.readline()
    ss_line = stdin.readline()
    ss = ss_line.strip().split(' ')
    head = ss[0]
    return (head, ss[1:])


def eval(t: tuple[str, list[str]]) -> bool:
    head, body = t
    
    def isProp(s: str) -> bool:
        match s.lower():
            case "at" | "by" | "for" | "from" | "in" | "of" | "on" | "to":
                return True
            case _:
                return False
    
    def checkAllLower(s: str) -> bool:
        return s.islower()
    
    def helper(cond: bool, lst: list[str]) -> bool:
        match (cond, lst):
            case (False, _):
                return False
            case (True, []):
                return True
            case (True, [curr, *rest]):
                return helper(checkAllLower(curr) if isProp(curr) else curr[0].isupper(), rest)
    
    head_cond = (
        checkAllLower(head[1:]) and head[0].isupper()
        if isProp(head)
        else head[0].isupper()
    )
    return helper(head_cond, body)


def print_(b: bool) -> None:
    match b:
        case True:
            print("Yes")
        case False:
            print("No")


def main() -> None:
    print_(eval(read()))


if __name__ == "__main__":
    main()
