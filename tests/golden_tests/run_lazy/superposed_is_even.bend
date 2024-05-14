data N = (S pred) | Z
data B = T | F

(Not B/T) = B/F
(Not B/F) = B/T

(IsEven n) =
  match n {
    N/S: (Not (IsEven n.pred))
    N/Z: B/T
  }

N0 = N/Z
N1 = (N/S N0)
N2 = (N/S N1)
N3 = (N/S N2)

Main = (IsEven {{N0 N1} {N2 N3}})
