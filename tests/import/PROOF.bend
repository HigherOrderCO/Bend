# a PROOF.bend fails when a law relies on unsafe code: its error lists
# the law, its own @unsafe def and the imported one no law names, and
# gives no hint, since moving the imported one out would not fix it
# (#1463; tests/proof/PROOF.bend shows the hint)
import Base
import ./LAWS.bend as Laws

def lie?() -> {1n == 0n : Nat}:
  lie()

def Laws.false_law():
  lie()

#|SOME PROOFS FAIL
#|Error: 3 defs rely on unsafe or foreign code:
#|defs that depend on @unsafe: unsafe-law/safe.spin, LAWS.false_law, lie
#|exit 1
