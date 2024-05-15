concat (String/Nil) str = str
concat (String/Cons c rest1) str2 = (String/Cons c (concat rest1 str2))

main = (concat "ab" "cd")
