main = @a @b
  let a = match a {
    String/Cons: 1
    String/Nil : 2
  };
  let b = match b {
    List/Cons: 1
    List/Nil : 2
  };
  (a, b)
