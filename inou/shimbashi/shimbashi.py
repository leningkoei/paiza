"""
```lean
def read : IO String := do
  let stdin ← IO.getStdin
  let input ← String.Slice.toString <$> String.trimAscii <$> stdin.getLine
  pure input

def eval (s : String) : Bool :=
  match s.toList with
  | [] => .false
  | 'a'::_ => .true
  | _::_ => .false

def print (bool : Bool) : IO Unit := IO.println $
  match bool with
  | .true => "Yes"
  | .false => "No"

def main : IO Unit := print =<< eval <$> read
```
"""

def read() -> str:
    import sys
    return sys.stdin.readline().rstrip("\n\r")


def eval(s: str) -> bool:
    if s == "":
        return False
    if s[0] == "a":
        return True
    return False


def print_(bool_val: bool) -> None:
    print("Yes" if bool_val else "No")


def main() -> None:
    print_(eval(read()))

if __name__ == "__main__":
    main()
