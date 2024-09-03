main: _ = 
use inf = (** 9.0 (** 9.0 9.0))
(
  (i24_to_u24 -1),
  (i24_to_u24 +1),
  (i24_to_u24 +0),
  (i24_to_u24 +400),
  (f24_to_u24 1.0),
  (f24_to_u24 1.5),
  (f24_to_u24 -3.0),
  (f24_to_u24 -3.5),
  (f24_to_u24 inf), # inf
  (f24_to_u24 (* -1.0 inf)), # -inf
  (f24_to_u24 (/ inf inf)), # nan (inf/inf)
  99999,
  (u24_to_i24 0),
  (u24_to_i24 1),
  (u24_to_i24 123456),
  (f24_to_i24 1.0),
  (f24_to_i24 1.5),
  (f24_to_i24 -3.0),
  (f24_to_i24 -3.5),
  (f24_to_i24 inf), # inf
  (f24_to_i24 (* -1.0 inf)), # -inf
  (f24_to_i24 (/ inf inf)), # nan (inf/inf)
  99999,
  (u24_to_f24 0),
  (u24_to_f24 1),
  (u24_to_f24 123456),
  (i24_to_f24 -1),
  (i24_to_f24 +1),
  (i24_to_f24 +0),
  (i24_to_f24 +400)
)