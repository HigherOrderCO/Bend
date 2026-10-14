type Unit<-T: Type> is Data:
  U{}

type In is Data:
  Mk{}

def idt(~A: Type, x: A) -> A:
  x

def main() -> In:
  idt(~In, Mk{})

#|Mk{}
