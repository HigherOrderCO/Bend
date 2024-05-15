# We want to make sure that the default value is not mistakenly erased in the first level of flattening.
data Maybe = (Some x) | None

(DoubleUnwrap (Maybe/Some (Maybe/Some x)) *) = x
(DoubleUnwrap * x) = x

Main = (DoubleUnwrap (Maybe/Some Maybe/None) 5)
