type List_ = (Cons x xs) | (Nil)

head x =
  match x {
  | List_/Nil: List_/Nil
  List_/Cons: x.x;
  }
