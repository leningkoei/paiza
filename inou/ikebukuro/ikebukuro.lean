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
    else toString n

def print : List String → IO (List Unit) := List.mapM
  λ s ↦ IO.println s

def main : IO Unit := Functor.discard $ print =<< eval <$> read
