# We want to make sure that the default value is not mistakenly erased in the first level of flattening.
type Maybe_ = (Some x) | None

(DoubleUnwrap (Maybe_/Some (Maybe_/Some x)) *) = x
(DoubleUnwrap * x) = x

Main = (DoubleUnwrap (Maybe_/Some Maybe_/None) 5)
