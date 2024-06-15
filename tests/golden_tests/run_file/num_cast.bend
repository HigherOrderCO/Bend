main = 
use inf = (** 9.0 (** 9.0 9.0))
(
  (to_u24 0),
  (to_u24 1),
  (to_u24 123456),
  (to_u24 -1),
  (to_u24 +1),
  (to_u24 +0),
  (to_u24 +400),
  (to_u24 1.0),
  (to_u24 1.5),
  (to_u24 -3.0),
  (to_u24 -3.5),
  (to_u24 inf), # inf
  (to_u24 (* -1.0 inf)), # -inf
  (to_u24 (/ inf inf)), # nan (inf/inf)
  99999,
  (to_i24 0),
  (to_i24 1),
  (to_i24 123456),
  (to_i24 -1),
  (to_i24 +1),
  (to_i24 +0),
  (to_i24 +400),
  (to_i24 1.0),
  (to_i24 1.5),
  (to_i24 -3.0),
  (to_i24 -3.5),
  (to_i24 inf), # inf
  (to_i24 (* -1.0 inf)), # -inf
  (to_i24 (/ inf inf)), # nan (inf/inf)
  99999,
  (to_f24 0),
  (to_f24 1),
  (to_f24 123456),
  (to_f24 -1),
  (to_f24 +1),
  (to_f24 +0),
  (to_f24 +400),
  (to_f24 1.0),
  (to_f24 1.5),
  (to_f24 -3.0),
  (to_f24 -3.5),
  (to_f24 inf), # inf
  (to_f24 (* -1.0 inf)), # -inf
  (to_f24 (/ inf inf)) # nan (inf/inf)
)