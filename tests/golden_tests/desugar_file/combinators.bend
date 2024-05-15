(foo) = λa λb λc (foo a)

(bar) = λa λb (a bar b)

(List/ignore list ignore) =
  match list {
    List/Cons: (List/ignore list.tail (List/ignore))
    List/Nil: 0
  }

(baz) = {0 1 2 3 λa a foo}

(qux) = {0 qux}

(clax) = (λx x λa λb λc λd (clax d))

(tup) = (tup, 1, 0)

(list) = [0 list]

(A x) = (let {a b} = A; λc (a b c) x)

(B x) = (let (a, b) = B; λc (a b c) x)

(Main) = list
