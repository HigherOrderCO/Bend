# HVM5 in Bend -- the laws. Claims about the pure logic under the
# evaluator: the lexer (a word and the rest re-form the source, a
# blank skip only moves the cursor), the parser's cursor (drops
# compose, a read past a drop is a read at the sum), the printer (a
# pack of n constructors prints n more braces than its body), and
# the closed facts about run words, name codes and the book's depth.
# PROOF.bend proves them.

import Base
import ./main.bend as Hvm

# LAW: the lexer loses nothing: for any source and any head class,
# the word at the head and the rest re-form the source.
law word_joins:
  for +s: String
  for z: Bool
  {Hvm.word_cat(Hvm.str_word(s, z)) == s : String}

# LAW: skipping blanks and comments only moves the cursor forward:
# for any source and any head class, the skipped source is the source
# past some n characters (n is the witness).
law skip_is_drop:
  for +s: String
  for z: Hvm.Ws
  exs n: Nat
  {Hvm.str_skip(s, z) == Hvm.str_drop(s, n) : String}

# LAW: the printer's packs add opening braces: the display of a pack
# of run p around x opens run_len(p) more than the display of x, for
# any run word and any term. Closing braces are not constrained here.
law pak_braces:
  for +p: U32
  for +x: Hvm.Term
  {Nat.add(Hvm.run_len(p), Hvm.opens(Hvm.term_show(x)))
    == Hvm.opens(Hvm.term_show(Hvm.Pak{p, x})) : Nat}

# LAW: the cursor composes: dropping m characters, then n, drops
# m + n.
law drop_drop:
  for s: String
  for m: Nat
  for n: Nat
  {Hvm.str_drop(Hvm.str_drop(s, m), n) == Hvm.str_drop(s, Nat.add(m, n)) : String}

# LAW: the code at k past a cursor moved by n is the code at n + k.
law at_drop:
  for s: String
  for n: Nat
  for k: Nat
  {Hvm.str_at(Hvm.str_drop(s, n), k) == Hvm.str_at(s, Nat.add(n, k)) : U32}

# LAW: a bit pushed outermost onto a run word is the bit bit0 reads
# back, and popping it restores the word (here the one-bit run #1{..},
# for either bit) (sanity).
law run_push_pop:
  for +b: Bool
  {(Hvm.bit0(Hvm.run_push(Hvm.run_word(1, 1), Hvm.pick(U32, b, 1, 0))),
    Hvm.run_pop(Hvm.run_push(Hvm.run_word(1, 1), Hvm.pick(U32, b, 1, 0))))
   == (Bool.not(b), Hvm.run_word(1, 1)) : Bool & U32}

# LAW: a name survives its code: the parser's six-bits-per-character
# code of a name prints back as the name (sanity).
law name_round_trip:
  {(Hvm.nam(Hvm.name_code("main", 0)), Hvm.nam(Hvm.name_code("F", 0)))
   == ("main", "F") : String & String}

# LAW: the book's depth is the least d with 2^d slots for n
# definitions (sanity).
law book_depth_fits:
  {(Hvm.book_depth(32n, 1, 0n), Hvm.book_depth(32n, 5, 0n), Hvm.book_depth(32n, 8, 0n),
    Hvm.book_depth(32n, 9, 0n))
   == (0n, 3n, 3n, 4n) : Nat & Nat & Nat & Nat}
