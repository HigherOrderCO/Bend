# Expected result: -0.349
hvm log_:
  (x ($([|] $(x ret)) ret))

hvm atan2_:
  ($([&] $(y ret)) (y ret))

main = (log_ (atan2_ 1.0 1.0) 2.0)