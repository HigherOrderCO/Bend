# io (io_marshal_imported_nullary_type): a module whose effect lays values
# of the module's own type, imported by io_marshal_imported_nullary. Like
# io_far_types, but its Off is nullary: a nullary constructor's own name
# was read off its type, which carries no source to read it from, so the
# namespace stayed on and neither lane named it as the module does.
# Alone it has no main: checked and interpreted only.
import Base

type Tag is Data:
  Off{}
  On{n: U32}

law tag.name:
  for t: Tag
  String

def tag.name(t):
  match t:
    case Off{}:
      "off"
    case On{n}:
      "on"

law tag.off:
  IO(Tag)

def tag.off():
  import "./marshal_imported_nullary_type.c"
  import "./marshal_imported_nullary_type.js"

law tag.on:
  IO(Tag)

def tag.on():
  import "./marshal_imported_nullary_type.c"
  import "./marshal_imported_nullary_type.js"

#|SOME PROOFS FAIL
#|Error: 2 defs rely on unsafe or foreign code:
#|- tag.off: foreign
#|- tag.on: foreign
#|exit 1
