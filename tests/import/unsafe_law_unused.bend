# a law filled safely beside an @unsafe def nothing names: the law holds,
# but the @unsafe def is a def of the book, so it fails the verdict
import Base
import ./unsafe-law/safe.bend as S

#|SOME PROOFS FAIL
#|Error: 1 def relies on unsafe or foreign code:
#|- unsafe-law/safe.spin: @unsafe
#|exit 1
