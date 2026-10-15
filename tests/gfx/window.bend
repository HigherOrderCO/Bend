# gfx (gfx_window): build(6n) is the full-detail 64 x 64 Image whose
# pixel (x, y) is x + 64y; fold sums its leaves (+q is shared). Without a
# display (the JS backend, a node with no session)
# Window.open Fails with one line and ENOTSUP on every lane and the frames
# fold unshown; on a display it requests fullscreen twice, frames each
# image (the shared one twice), releases the grab, requests windowed twice
# and frames again before close. Unsupported fullscreen retains the handle.
import Base

def count(xs: List<Event>) -> U32:
  match xs:
    case Nil{}:
      0
    case e <> rest:
      U32.inc(count(rest))

def build(+d: Nat, +x: U32, +y: U32) -> Image:
  match d:
    case 0n:
      Pix{(x + 64 * y : U32)}
    case 1n+e:
      +h = U32.shln(1, e)
      tl tr bl br = build(e, x, y) build(e, (x + h : U32), y)
        build(e, x, (y + h : U32)) build(e, (x + h : U32), (y + h : U32))
      Qua{tl, tr, bl, br}

def fold(im: Image) -> U32:
  match im:
    case Pix{c}:
      c
    case Qua{tl, tr, bl, br}:
      a b c d = fold(tl) fold(tr) fold(bl) fold(br)
      (a + b + c + d : U32)

def seen(q: Window & Image & List<Event>) -> IO(Window & Image):
  (w, +im, evs) = q
  do IO<Window & Image>:
    u : Unit <- IO.print(U32.show(count(evs)) ++ " events, fold "
      ++ U32.show(fold(im)))
    IO.pure(Window & Image, (w, im))

def frames(n: Nat, p: Window & Image) -> IO(Window):
  match n:
    case 0n:
      (w, im) = p
      IO.pure(Window, w)
    case 1n+k:
      (w, im) = p
      do IO<Window>:
        q : Window & Image & List<Event> <- Window.frame(w, im)
        r : Window & Image <- seen(q)
        frames(k, r)

def close(w: Window) -> IO(Unit):
  Window.close(w)

def full_answer(p: Window & Result<&1, &1, U32 & String, Unit>) -> IO(Window):
  (w, r) = p
  match r:
    case Done{u}:
      IO.pure(Window, w)
    case Fail{(code, text)}:
      do IO<Window>:
        IO.print("fullscreen errno " ++ U32.show(code))
        IO.pure(Window, w)

def set_full(w: Window, on: Bool) -> IO(Window):
  do IO<Window>:
    p : Window & Result<&1, &1, U32 & String, Unit> <- Window.fullscreen(w, on)
    full_answer(p)

def shown(w: Window) -> IO(Unit):
  +q = build(2n, 0, 0)
  do IO<Unit>:
    wf : Window <- set_full(w, True{})
    wr : Window <- set_full(wf, True{})
    w1 : Window <- frames(1n, (wr, Pix{7}))
    w2 : Window <- frames(1n, (w1, build(6n, 0, 0)))
    g : Window <- Window.grab(w2, True{})
    w3 : Window <- frames(2n, (g, Qua{q, Pix{1}, q, Pix{2}}))
    w4 : Window <- Window.grab(w3, False{})
    wa : Window <- set_full(w4, False{})
    wb : Window <- set_full(wa, False{})
    w5 : Window <- frames(1n, (wb, Pix{7}))
    close(w5)

def sup(yes: Bool, code: U32) -> String:
  match yes:
    case True{}:
      "ENOTSUP"
    case False{}:
      U32.show(code)

def refused(+code: U32, text: String) -> IO(Unit):
  +q = build(2n, 0, 0)
  do IO<Unit>:
    u : Unit <- IO.print(text)
    v : Unit <- IO.print(sup(U32.is_eq(code, 45) || U32.is_eq(code, 95), code))
    x : Unit <- IO.print(U32.show(fold(Pix{7})))
    y : Unit <- IO.print(U32.show(fold(build(6n, 0, 0))))
    IO.print(U32.show(fold(Qua{q, Pix{1}, q, Pix{2}})))

def opened(r: Result<&1, &1, U32 & String, Window>) -> IO(Unit):
  match r:
    case Fail{(code, text)}:
      refused(code, text)
    case Done{w}:
      shown(w)

def main() -> IO(Unit):
  do IO<Unit>:
    r : Result<&1, &1, U32 & String, Window> <- Window.open("gfx", 64, 48)
    opened(r)

#|Window.open: no display (build a native binary with bend <file> -o <out> and run it from a desktop session)
#|ENOTSUP
#|7
#|8386560
#|3123
