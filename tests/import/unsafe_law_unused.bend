# a law filled safely beside an @unsafe def nothing names: the law holds,
# but the @unsafe def is a def of the book, so it fails the verdict
import Base
import ./unsafe-law/safe.bend as S

#|SOME PROOFS FAIL
#|Error: 1 def relies on unsafe or foreign code:
#|defs that depend on @unsafe: unsafe-law/safe.spin
#|Hint: no law or proof uses these defs. Move them to a file this proof does not import, and the proofs will check: unsafe-law/safe.spin
#|exit 1
