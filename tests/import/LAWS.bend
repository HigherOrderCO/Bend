# the laws tests/import/PROOF.bend proves (#1463): a false one, beside a
# module whose @unsafe def no law names. Alone, the law is an open claim
import Base
import ./unsafe-law/safe.bend as S

law false_law:
  {1n == 0n : Nat}

#|SOME PROOFS FAIL
#|Error: 1 TODO found.
#|The code is incomplete, and not a valid proof yet.
#|exit 1
