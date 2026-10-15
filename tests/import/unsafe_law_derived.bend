# a law proved from an unsafely filled law relies on unsafe too, with no
# @unsafe of its own; both laws are listed, and the helper boom (#1001)
import Base
import ./unsafe-law/derived.bend as D

#|SOME PROOFS FAIL
#|Error: 3 defs rely on unsafe or foreign code:
#|defs that depend on @unsafe: unsafe-law/laws.false_law, unsafe-law/succ.succ_is_id, unsafe-law/derived.boom
#|exit 1
