# proof_userland_tactics: a tactic, from userland. Propositions are
# embedded as data (P), Interp turns them into native types, and one
# total function auto(p) -> Maybe<Interp(p)> decides them: Some carries
# the proof term, None is the honest refusal. The model is min.
#
#   bend demos/proof_userland_tactics/main.bend
import Base

# --- the model the laws are about: min, by double structural recursion ---
def min(a: Nat, b: Nat) -> Nat:
  match a:
    case 0n:
      0n
    case 1n+p:
      match b:
        case 0n:
          0n
        case 1n+q:
          1n+min(p, q)

# --- the tactic framework ---
# proposition syntax as data
type P is Data:
  PTrue{}
  PLe{a: Nat, b: Nat}
  PAnd{l: P, r: P}

# interpretation: syntax -> native proposition types. A proof of
# Interp(p) IS a proof of the native proposition; PLe's atoms are
# native terms, so Interp(PLe{a, b}) normalizes to the goal itself.
def Interp(p: P) -> Type:
  match p:
    case PTrue{}:
      Unit
    case PLe{a, b}:
      {Nat.is_le(a, b) == True{} : Bool}
    case PAnd{l, r}:
      Interp(l) & Interp(r)

# decider for the PLe leaf
def zero_le(q: Nat) -> {Nat.is_le(0n, q) == True{} : Bool}:
  match q:
    case 0n:
      {==}
    case 1n+qq:
      {==}

def prove_le(a: Nat, b: Nat) -> Maybe<{Nat.is_le(a, b) == True{} : Bool}>:
  match a:
    case 0n:
      Some{zero_le(b)}
    case 1n+p:
      match b:
        case 0n:
          None{}
        case 1n+q:
          prove_le(p, q)

# and-case helper (a match scrutinizes a parameter, not a computed value)
def auto_and(-l: P, -r: P, ml: Maybe<Interp(l)>, mr: Maybe<Interp(r)>) -> Maybe<(Interp(l) & Interp(r))>:
  match ml:
    case Some{wl}:
      match mr:
        case Some{wr}:
          Some{(wl, wr)}
        case None{}:
          None{}
    case None{}:
      None{}

# the tactic: total over the syntax; Some{...} proves, None{} refuses
def auto(p: P) -> Maybe<Interp(p)>:
  match p:
    case PTrue{}:
      Some{Unit{}}
    case PLe{a, b}:
      prove_le(a, b)
    case PAnd{l, r}:
      auto_and(l, r, auto(l), auto(r))

# the agent-facing signal: a proof term is not printable; its presence
# or absence is the message
def tag(-p: P, m: Maybe<Interp(p)>) -> String:
  match m:
    case Some{w}:
      "Some"
    case None{}:
      "None"

# goals the tactic meets: both min facts, and a capped claim; 9 <= 5
# is refused
def goal_both() -> P:
  PAnd{PLe{min(100n, 200n), 200n}, PLe{min(7n, 100n), 100n}}

def goal_capped() -> P:
  PLe{min(300n, 200n), 200n}

def goal_false() -> P:
  PAnd{PTrue{}, PLe{9n, 5n}}

def main() -> IO(Unit):
  IO.print(tag(goal_both(), auto(goal_both())) ++ " "
    ++ tag(goal_capped(), auto(goal_capped())) ++ " "
    ++ tag(goal_false(), auto(goal_false())))
