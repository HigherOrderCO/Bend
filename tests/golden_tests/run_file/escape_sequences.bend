String/from_list [] = ""
String/from_list (List/Cons x xs) = (String/Cons x (String/from_list xs))

(Concat String/Nil         ys) = ys
(Concat (String/Cons x xs) ys) = (String/Cons x (Concat xs ys))

(Join List/Nil)         = ""
(Join (List/Cons x xs)) = (Concat x (Join xs))

(Expand xs) = fold xs { String/Nil: xs; String/Cons: xs }

main =
  let a = (String/from_list ['\n', '\r', '\t', '\0', '\"', '\'', '\u{AFE}', '\\'])
  let b = (Join ["\n", "\r", "\t", "\0", "\"", "\'", "\u{AFE}", "\\"])
  (Expand (Concat a b))
