# Slash Boss 3D -- the proofs. This file imports the game (as Slash)
# and the claims (as Laws) and fills every law. pause_freezes is an
# induction over the ticks: one tick of a paused game with no key
# tapped routes to the rest (no flag is up: the taps are 0 and the
# mode is 4), and the rest of a paused game keeps every field but the
# flash (pause_tick, by evaluation of step, route and rest); the
# remaining ticks are the induction hypothesis at the faded flash.
# pause_twice splits the mode's two tests as Bools (toggle_twice):
# paused resumes then pauses, fighting pauses then resumes, any other
# mode stays, shown by rewriting the tests back into the toggled
# mode. esc_quits is an induction over the events before the Esc; the
# Key arm is general over the key's quit flag. The sanity laws
# compute.

import Base
import ./bend3d.bend as R
import ./main.bend as Slash
import ./LAWS.bend as Laws

# A pause freezes the world
# -------------------------

# one tick of a paused game with no key tapped: only the flash fades
law pause_tick:
  for +time: U32
  for +held: U32
  for +hero: Slash.Fighter
  for +boss: Slash.Boss
  for +flash: U32
  for +cloth: R.Cloth
  for +garb: Slash.Garb
  for +lens: Slash.Lens
  for +blast: Maybe<&2, Slash.Blast>
  {Slash.Game{time, held, 0, 4, hero, boss, Slash.Sim.dec(flash), cloth, garb, lens, blast}
    == Slash.Game.step(Slash.Game{time, held, 0, 4, hero, boss, flash, cloth, garb, lens, blast})
    : Slash.Game}

def pause_tick(time, held, hero, boss, flash, cloth, garb, lens, blast):
  {==}

def Laws.pause_freezes(n, time, held, hero, boss, flash, cloth, garb, lens, blast):
  match n:
    case 0n:
      {==}
    case 1n+k:
      %pause_tick(time, held, hero, boss, flash, cloth, garb, lens, blast)
        : {Slash.Game.steps(k, _)
           == Slash.Game{time, held, 0, 4, hero, boss, Slash.Sim.fade(k, Slash.Sim.dec(flash)),
                cloth, garb, lens, blast} : Slash.Game}
      Laws.pause_freezes(k, time, held, hero, boss, Slash.Sim.dec(flash), cloth, garb, lens, blast)

# P twice restores the pause state
# --------------------------------

# the mode's two tests split: paused (4) goes to 1 and back to 4,
# fighting (1) goes to 4 and back to 1, any other mode stays itself
law toggle_twice:
  for paused: Bool
  for fighting: Bool
  for +mode: U32
  for +hp: {paused == U32.is_eq(mode, 4) : Bool}
  for hf: {fighting == U32.is_eq(mode, 1) : Bool}
  {U32.is_eq(Slash.Mode.toggle(Slash.Mode.toggle.go(paused, fighting, mode)), 4) == paused : Bool}

def toggle_twice(paused, fighting, mode, hp, hf):
  match paused fighting:
    case True{} _:
      {==}
    case False{} True{}:
      {==}
    case False{} False{}:
      %hp : {U32.is_eq(Slash.Mode.toggle.go(_, U32.is_eq(mode, 1), mode), 4) == False{} : Bool}
      %hf : {U32.is_eq(Slash.Mode.toggle.go(False{}, _, mode), 4) == False{} : Bool}
      %hp : {_ == False{} : Bool}
      {==}

def Laws.pause_twice(g):
  match g:
    case Slash.Game{time, held, taps, mode, hero, boss, flash, cloth, garb, lens, blast}:
      toggle_twice(U32.is_eq(mode, 4), U32.is_eq(mode, 1), mode, {==}, {==})

# Esc anywhere
# ------------

law esc_arm:
  for b: Bool
  for g: Slash.Game
  for -rest: List<Event>
  for ih: @s: Slash.Game -> {Slash.Game.events(rest, s) == None{} : Maybe<Slash.Game>}
  {Maybe.bind(&1, Slash.Game, Slash.Game, Slash.Game.quit(b, g),
      s => Slash.Game.events(rest, s)) == None{} : Maybe<Slash.Game>}

def esc_arm(b, g, rest, ih):
  match b:
    case True{}:
      {==}
    case False{}:
      ih(g)

def Laws.esc_quits(pre, post, g):
  match pre:
    case Nil{}:
      {==}
    case Key{+code, +down} <> rest:
      esc_arm(U32.is_eq(code, 27), Slash.Game.key(g, code, down),
        Slash.Game.cat(rest, Key{27, True{}} <> post),
        s => Laws.esc_quits(rest, post, s))
    case Text{text} <> rest:
      Laws.esc_quits(rest, post, g)
    case Mouse{x, y, button, down} <> rest:
      Laws.esc_quits(rest, post, g)
    case Move{x, y} <> rest:
      Laws.esc_quits(rest, post, g)
    case Look{dx, dy} <> rest:
      Laws.esc_quits(rest, post, g)
    case Scroll{x, y, dx, dy} <> rest:
      Laws.esc_quits(rest, post, g)
    case Close{} <> rest:
      {==}

# Sanity
# ------

def Laws.back_to_title(fresh, pause, live, frozen, g):
  {==}

def Laws.pause_toggles():
  {==}

def Laws.dec_floor():
  {==}

def Laws.arrows_are_wasd():
  {==}
