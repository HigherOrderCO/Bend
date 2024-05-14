// Testing various forms of pattern matching
data Result_ = (Ok val) | (Err err)

Parse state (String/cons '(' xs) = (Result_/Ok ('(', xs, state))
Parse state (String/cons ')' xs) = (Result_/Ok (')', xs, state))
Parse state (String/cons '\n' xs) = (Result_/Ok (0, xs, state))
Parse state xs = (Result_/Err (xs, state))

main =
  let str = "(+";
  let state = *;
  match res = (Parse state str) {
    Result_/Ok: let (val, xs, state) = res.val; (val, (Parse state xs))
    err: err
  }
