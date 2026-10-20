# a proof fails when any def it imports relies on unsafe or foreign code,
# even one no law or proof uses (#1463). Here only the main module's
# @unsafe loop does, so the error ends with a hint naming the defs to
# move out of the proof's imports. tests/import/PROOF.bend shows a law
# that relies on one: no hint there
import Base
import ./LAWS.bend as Laws

def Laws.greets():
  {==}

#|SOME PROOFS FAIL
#|Error: 2 defs rely on unsafe or foreign code:
#|defs that depend on @unsafe: app/main.serve, app/main.main
#|Hint: no law or proof uses these defs. Move them to a file this proof does not import, and the proofs will check: app/main.serve, app/main.main
#|exit 1
