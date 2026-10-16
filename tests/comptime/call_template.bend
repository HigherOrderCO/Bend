# An error in the body appears only when this template is instantiated.
def call(~f: Quant -> Quant) -> Quant:
  f(&1, &2)

#|All terms check.
