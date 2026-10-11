# Test the Linux audio pump through the native Audio effects.
# The C test uses ALSA's null output and injects device return values.
import Base

def AudioTest.mode() -> IO(U32):
  import "./audio_linux_progress.c"
  import "./audio_linux_progress.js"

def AudioTest.prepare(mode: U32, width: U32) -> IO(Unit):
  import "./audio_linux_progress.c"
  import "./audio_linux_progress.js"

def AudioTest.wait(mode: U32) -> IO(Unit):
  import "./audio_linux_progress.c"
  import "./audio_linux_progress.js"

def AudioTest.closed() -> IO(Unit):
  import "./audio_linux_progress.c"
  import "./audio_linux_progress.js"

def samples(n: Nat, +i: U32) -> List<&2, F32>:
  match n:
    case 0n:
      []
    case 1n+p:
      +x = U32.to_f32(i)
      x <> ((0.0 - x : F32) <> samples(p, (i + 1 : U32)))

def close_pair(pair: Audio & U32) -> IO(Unit):
  (a, queued) = pair
  Audio.close(a)

def failed_call(close: Bool, a: Audio) -> IO(Unit):
  match close:
    case True{}:
      Audio.close(a)
    case False{}:
      do IO<Unit>:
        pair : Audio & U32 <- Audio.write(a, [])
        close_pair(pair)

def outcome(good: Bool, mode: U32, a: Audio) -> IO(Unit):
  match good:
    case True{}:
      do IO<Unit>:
        Unit <- Audio.close(a)
        AudioTest.closed()
    case False{}:
      do IO<Unit>:
        Unit <- IO.print("before")
        Unit <- failed_call(U32.is_eq(mode, 9), a)
        IO.print("after")

def finish(+mode: U32, pair: Audio & U32) -> IO(Unit):
  (a, queued) = pair
  do IO<Unit>:
    Unit <- AudioTest.wait(mode)
    outcome(Bool.or(U32.is_lt(mode, 8), U32.is_gt(mode, 14)), mode, a)

def consume(+mode: U32, width: U32) -> IO(Unit):
  do IO<Unit>:
    Unit <- AudioTest.prepare(mode, width)
    a : Audio <- IO.try(Audio, Audio.open(48000))
    pair : Audio & U32 <- Audio.write(a, samples(512n, 1))
    finish(mode, pair)

def shorts(n: Nat) -> IO(Unit):
  match n:
    case 0n:
      IO.pure(Unit, Unit{})
    case 1n+ +p:
      do IO<Unit>:
        Unit <- consume(1, U32.from_nat(1n+p))
        shorts(p)

def child(matrix: Bool, mode: U32) -> IO(Unit):
  match matrix:
    case True{}:
      shorts(255n)
    case False{}:
      consume(mode, 256)

def selected(parent: Bool, +mode: U32) -> IO(Unit):
  match parent:
    case True{}:
      IO.print("audio")
    case False{}:
      child(U32.is_eq(mode, 1), mode)

def main() -> IO(Unit):
  do IO<Unit>:
    +mode : U32 <- AudioTest.mode()
    selected(U32.is_eq(mode, 0), mode)

#|audio
