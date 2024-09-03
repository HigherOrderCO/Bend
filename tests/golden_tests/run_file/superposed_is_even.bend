type MyNat = (S pred) | Z
type MyBool = T | F

(Not MyBool/T) = MyBool/F
(Not MyBool/F) = MyBool/T

(IsEven n) =
  match n {
    MyNat/S: (Not (IsEven n.pred))
    MyNat/Z: MyBool/T
  }

N0 = MyNat/Z
N1 = (MyNat/S N0)
N2 = (MyNat/S N1)
N3 = (MyNat/S N2)

Main = (IsEven {{N0 N1} {N2 N3}})
