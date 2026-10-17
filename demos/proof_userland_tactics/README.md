# proof_userland_tactics

Bend has no tactics — and a tactic is still a library away. This demo
builds one from userland, on stock 2.0.37:

- **propositions as data**: `P` (`PTrue` / `PLe` / `PAnd`)
- **`Interp : P -> Type`** turns syntax into native goals. A proof of
  `Interp(p)` IS a proof of the native proposition; `Interp(PLe{a, b})`
  normalizes to `{Nat.is_le(a, b) == True}` itself, so the tactic's
  atoms are native terms — no glue
- **`auto(p) -> Maybe<Interp(p)>`**: one total function that decides
  the fragment. `Some{...}` carries the proof term; `None{}` is the
  honest refusal an agent can act on

The load-bearing feature is a computed type family in a signature:
`Maybe<Interp(p)>` as the return type of a generic function. No quote,
no unify, no compiler changes — reflection, not metaprogramming.

## The two roles

- **quantified laws stay hand-proved** (`PROOF.bend`): `min_le` by
  structural induction where the self-call is the induction hypothesis;
  `le_succ_r`, the wall lemma, by induction plus a contradiction
  (`is_le(1n+p, 0n)` refutes through `Empty`)
- **instances and conjunctions go through the tactic**: `Tac.auto(...)`
  discharges native-stated goals definitionally, or refuses

## Run it

```
$ bend demos/proof_userland_tactics/main.bend
Some Some None
```

Both min facts prove (`Some`), the capped claim `min(300, 200) <= 200`
proves (`Some`), and `PLe{9, 5}` is refused (`None`).

Now introduce a bug — let `min` return its left argument when the right
one runs out (`case 0n: 1n+p` instead of `0n` in `main.bend`):

```
$ bend demos/proof_userland_tactics/main.bend
Some None None

$ bend demos/proof_userland_tactics/PROOF.bend
SOME PROOFS FAIL
```

The tactic flips `Some -> None` — the unprovable goal is named, not
guessed around — and the quantified law fails with it. That `None` is
the agent-facing signal: not "error", but "here is what I could not
prove".

## Limits, honestly

The fragment is what you embed. Order facts and conjunctions are here;
decidable arithmetic (addition, quantifiers over Nat) fits the same
recipe and would close the `le_succ_r` class automatically. Goals over
arbitrary native types still meet "Bend has no tactics": there is no
quote of a Type (`match A` on a Type parameter is a dead-region error)
and no goal introspection, so the framework reaches exactly the
fragment you embed, soundly, and nothing else.
