# We want to make sure that the default value is not mistakenly erased in the first level of flattening.
(DoubleUnwrap (Maybe/Some (Maybe/Some val)) *) = val
(DoubleUnwrap * x) = x

Main = (DoubleUnwrap (Maybe/Some Maybe/None) 5)
