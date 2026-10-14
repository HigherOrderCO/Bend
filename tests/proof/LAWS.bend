# the laws PROOF.bend proves (#1463): one about the pure greet, beside
# the main module, whose @unsafe loop no law names, so PROOF.bend fails
# with a hint. Alone, the law is an open claim
import Base
import ./app/greet.bend as G
import ./app/main.bend as M

law greets:
  {G.greet("x") == "hi x" : String}

#|SOME PROOFS FAIL
#|Error: 1 TODO found.
#|The code is incomplete, and not a valid proof yet.
#|exit 1
