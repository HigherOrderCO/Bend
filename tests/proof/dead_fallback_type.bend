# a fallback past a type's last constructor is dead (WONTFIX.txt), in a
# type as in a body: --verdict skips it as bend2 does, so t reaches the
# kernel instead of going out of scope as an application with no known
# head type. An annotated match is a fallback too (u): only a bare match
# continues the chain
import Base

def t(u: Unit) -> {{\{Unit: 3n; 0n} : Unit -> Nat}(Unit{}) == 3n : Nat}:
  {==}

def u(v: Unit) -> {{\{Unit: 3n; {\{Unit: 1n} : Unit -> Nat}} : Unit -> Nat}(Unit{}) == 3n : Nat}:
  {==}

def main() -> Nat:
  {\{Unit: 3n; 0n} : Unit -> Nat}(Unit{})

#|3n
