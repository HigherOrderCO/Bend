checked (scott_concat a b) = (a
  λh λt λcons λnil (cons h (scott_concat t b))
  b
)
main = scott_concat