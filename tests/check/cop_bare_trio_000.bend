# a book that never imports Base declares its own Data Nat: the Data
# kind is the whole duplication license, so a + binder over Nat is
# accepted, and a match on a Many scrutinee hands its fields out at
# Many, so an arm may consume a field twice
type Nat is Data:
  Zero{}
  Succ{pred: Nat}

law Nat.add:
  for a: Nat
  for b: Nat
  Nat

def Nat.add(a, b):
  match a:
    case Zero{}:
      b
    case Succ{p}:
      Succ{Nat.add(p, b)}

law addself:
  for +x: Nat
  Nat

def addself(x):
  match x:
    case Zero{}:
      Zero{}
    case Succ{p}:
      Succ{Nat.add(p, p)}

law main:
  Nat

def main():
  addself(Succ{Succ{Zero{}}})

#|Succ{Succ{Succ{Zero{}}}}
