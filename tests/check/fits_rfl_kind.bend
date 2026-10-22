# soundness pin: term_compare's kind order (Data fits Type) is LE-only.
# Rfl claims equality, so it compares EQ: {==} must not prove
# {Data == Type} (the audit's e1 witness: J would transport it
# backwards into a Type -> Data cast and a closed Empty).
type Empty is Data:

law data_type: {Data == Type : Type}
def data_type(): {==}

#|SOME PROOFS FAIL
#|Error:
#|- message  : {==} needs both sides to compute to the same value
#|Location: data_type
#|7 | law data_type: {Data == Type : Type}
#|8>| def data_type(): {==}
#|  |                  ^^^^
#|9 |
#|- left     : Data  =>  Data
#|- right    : Type  =>  Type
#|exit 1
