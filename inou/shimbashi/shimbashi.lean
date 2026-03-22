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
