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
  let isPrep : String → Bool := λ s ↦ match s.toLower with
  | "at" | "by" | "for" | "from" | "in" | "of" | "on" | "to" => true
  | _ => false
  let checkAllLower : String → Bool := λ s ↦ s.all Char.isLower
  let rec helper : Bool → List String → Bool
  | false, _ => false
  | true, [] => true
  | true, curr :: rest =>
    helper (if isPrep curr then checkAllLower curr else curr.front.isUpper) rest
  helper (
    if isPrep head
      then checkAllLower (head.drop 1).toString && head.front.isUpper
      else head.front.isUpper
  ) body

def print : Bool → IO Unit
| true => IO.println "Yes"
| false => IO.println "No"

def main : IO Unit := Functor.discard $ print =<< eval <$> read
