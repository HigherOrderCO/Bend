# a module named after Base's Word family: its bare Nil and Con are List's.
# Its namespace was Word, so a bare name first tried Word.<name>, and Nil
# resolved to Base's Word.Nil (#1124). base_family_file.bend imports it.
import Base

def f() -> Nat:
  List.length(&1, U32, Con{7, Nil{}})

#|ALL PROOFS CHECK
#|Use --verdict for mathematical validity.
