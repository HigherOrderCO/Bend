type list = (cons h t) | nil

reverse (list/cons h t) = (concat (reverse t) (list/cons h list/nil))
reverse list/nil = list/nil

concat (list/cons h t) x = (list/cons h (concat t x))
concat list/nil x = x

main = (reverse (list/cons 3 (list/cons 2 (list/cons 1 list/nil))))
