# a law filled by an @unsafe def in an imported file is a claim of the
# check, though this file never names it: the verdict lists it (#1001)
import Base
import ./unsafe-law/fill.bend as F

#|SOME PROOFS FAIL
#|Error: 1 def relies on unsafe or foreign code:
#|- unsafe-law/laws.false_law: @unsafe
#|exit 1
