String/from_list [] = ""
String/from_list (List/cons x xs) = (String/cons x (String/from_list xs))

(Concat String/nil         ys) = ys
(Concat (String/cons x xs) ys) = (String/cons x (Concat xs ys))

(Join List/nil)         = ""
(Join (List/cons x xs)) = (Concat x (Join xs))

main =
  ((String/from_list ['\n', '\r', '\t', '\0', '\"', '\'', '\u{AFE}', '\\'])
  , (Join ["\n", "\r", "\t", "\0", "\"", "\'", "\u{AFE}", "\\"]))
