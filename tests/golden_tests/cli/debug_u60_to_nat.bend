type Nat_ = (Z) | (S nat)

U60ToNat n = switch n {
  0: Nat_/Z
  _: (Nat_/S (U60ToNat n-1))
}

(Main n) = (U60ToNat n)
