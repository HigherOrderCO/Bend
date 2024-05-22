type bool = true | false

not bool/false = bool/true
not bool/true = bool/false

and bool/false bool/false = bool/false
and bool/false bool/true = bool/false
and bool/true bool/false = bool/false
and bool/true bool/true = bool/true

and2 bool/true b = b
and2 bool/false b = bool/false

and3 bool/true bool/true = bool/true
and3 a b = bool/false

and4 bool/false b = bool/false
and4 a bool/false = bool/false
and4 a b = bool/true
