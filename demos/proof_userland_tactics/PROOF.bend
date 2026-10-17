# proof_userland_tactics: the proofs, in two roles.
#
# - quantified laws stay hand-proved: structural induction where the
#   self-call is the induction hypothesis
# - instances and conjunctions go through the tactic: Tac.auto builds
#   the proof term at run time, and its Maybe<Interp(p)> is the native
#   goal, definitionally — no glue
#
#   bend demos/proof_userland_tactics/PROOF.bend

import Base
import ./main.bend as Tac
import ./LAWS.bend as Laws

# a contradiction toolkit: {False == True} has no inhabitant
def disc(b: Bool) -> Type:
  match b:
    case False{}:
      {0n == 0n : Nat}
    case True{}:
      Empty

def contra(px: {False{} == True{} : Bool}) -> Empty:
  %px : {disc(_) : Type}
  {==}

# --- min_le, by induction on a ---
# 0 <= b by zero_le; min(1n+p, 0n) = 0n; and the 1/1 case strips both
# succs, so the self-call IS the goal — no strengthening needed.
def Laws.min_le(a, b):
  match a:
    case 0n:
      Tac.zero_le(b)
    case 1n+p:
      match b:
        case 0n:
          Tac.zero_le(0n)
        case 1n+q:
          Laws.min_le(p, q)

# --- le_succ_r: the wall lemma, by induction on x ---
# x = 0n: zero_le again. x = 1n+p with q = 0n: the hypothesis is
# impossible (is_le(1n+p, 0n) is False), refuted. Both succs: strip.
def Laws.le_succ_r(x, q, px):
  match x:
    case 0n:
      Tac.zero_le(1n+q)
    case 1n+p:
      match q:
        case 0n:
          Empty.absurd({Nat.is_le(1n+p, 1n+0n) == True{} : Bool}, contra(px))
        case 1n+r:
          Laws.le_succ_r(p, r, px)

# --- the tactic part: an instance goal, discharged by auto ---
# The stated type is the native proposition; Tac.auto(goal) returns
# Maybe<Interp(goal)> — the same type, because Interp(PLe{a, b}) IS
# {Nat.is_le(a, b) == True}.
def min_caps() -> Maybe<{Nat.is_le(Tac.min(300n, 200n), 200n) == True{} : Bool}>:
  Tac.auto(Tac.PLe{Tac.min(300n, 200n), 200n})
