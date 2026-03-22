"""
```lean
def read : IO (Nat × Nat × Nat) := do
  let stdin ← IO.getStdin
  let n :=
    match (← String.Slice.toNat? <$> String.trimAscii <$> stdin.getLine) with
    | .none => panic! "Error: Expect three nats"
    | .some n => n
  let a :=
    match (← String.Slice.toNat? <$> String.trimAscii <$> stdin.getLine) with
    | .none => panic! "Error: Expect three nats"
    | .some a => a
  let b :=
    match (← String.Slice.toNat? <$> String.trimAscii <$> stdin.getLine) with
    | .none => panic! "Error: Expect three nats"
    | .some b => b
  pure (n, a, b)

def eval : (Nat × Nat × Nat) → List String
| (n, a, b) => List.range' 1 n 1 |>.map
  λ n ↦
    if n % a == 0 && n % b == 0 then "paizaprogramming"
    else if n % a == 0 then "paiza"
    else if n % b == 0 then "programming"
    else ToString.toString n

def print : List String → IO (List Unit) := List.mapM
  λ s ↦ IO.println s

def main : IO Unit := Functor.discard $ print =<< eval <$> read
```
"""

import sys

def read() -> tuple[int, int, int]:
    n_line = sys.stdin.readline()
    a_line = sys.stdin.readline()
    b_line = sys.stdin.readline()
    n: int = int(n_line.strip())
    a: int = int(a_line.strip())
    b: int = int(b_line.strip())
    return (n, a, b)


def eval(t: tuple[int, int, int]) -> list[str]:
    n, a, b = t
    result: list[str] = []
    i: int = 1
    while i <= n:
        if i % a == 0 and i % b == 0:
            result.append("paizaprogramming")
        elif i % a == 0:
            result.append("paiza")
        elif i % b == 0:
            result.append("programming")
        else:
            result.append(str(i))
        i += 1
    return result


def print_(strings: list[str]) -> list[None]:
    return [print(s) for s in strings]


def main() -> None:
    print_(eval(read()))


if __name__ == "__main__":
    main()
