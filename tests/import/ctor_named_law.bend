# a module's law may use a def below it named like a Base constructor
# (Done): it states the module's Done. once it stated the bare Done, an
# importer's def of that name took its place, and the proof checked a
# law the module never stated (#1236)
import Base
import ./ctor-named/laws.bend as M

def Done(b: Bool) -> Bool:
  False{}

def M.keeps():
  {==}

#|ALL PROOFS CHECK
#|Use --verdict for mathematical validity.
