# an entry imports a sibling named after a Base family (Word.bend), whose
# bare Nil and Con stay List's, not Base's Word.Nil (#1124). A file's
# namespace is its path from the entry's directory, so only a sibling of
# the entry is named Word.
import Base
import ./Word.bend as W

def main() -> Nat:
  W.f()

#|1n
