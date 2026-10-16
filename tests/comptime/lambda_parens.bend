# Parentheses retain template lambda substitution, including in a nested call.
def apply(~f: Quant -> Quant, x: Quant) -> Quant:
  ((f))(x)

def compose(~f: Quant -> Quant, ~g: Quant -> Quant, x: Quant) -> Quant:
  apply(~(z => f(g(z))), x)

def main() -> Quant:
  compose(~(a => a), ~(b => b), &1)

#|&1
