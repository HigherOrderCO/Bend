# the unsafe fill written in the file under check: listed as before
import Base
import ./unsafe-law/laws.bend as Laws

def Laws.false_law?():
  Laws.false_law()

#|SOME PROOFS FAIL
#|Error: 1 def relies on unsafe or foreign code:
#|- unsafe-law/laws.false_law: @unsafe
#|exit 1
