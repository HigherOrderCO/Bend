type Type = A | B | C | D

Foo Type/A Type/B Type/C Type/D = 0
Foo Type/A Type/B Type/D Type/D = 1
Foo a b Type/D Type/D = 2

main = (Foo Type/A Type/B Type/C Type/D)
