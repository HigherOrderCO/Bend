# Whole-frame draining carries subframe time, but never carries idle time.
# JS only: the native ring follows its device rather than this test clock.
import Base

def AudioTest.clock(t: U32, reset: Bool) -> IO(Unit):
  import "./audio_frame_clock.js"

def samples(n: Nat, xs: List<&2, F32>) -> List<&2, F32>:
  match n:
    case 0n: xs
    case 1n+p: samples(p, 0.0 <> xs)

def report(pair: Audio & U32) -> IO(Audio):
  (next, queued) = pair
  do IO<Audio>:
    IO.print(U32.show(queued))
    return next

def write(audio: Audio, n: Nat) -> IO(Audio):
  do IO<Audio>:
    pair : Audio & U32 <- Audio.write(audio, samples(n, []))
    report(pair)

def main() -> IO(Unit):
  do IO<Unit>:
    AudioTest.clock(0, False{})
    a : Audio <- IO.try(Audio, Audio.open(8000))
    # One extra sample must reject the whole request, even after rounding.
    empty : Audio <- write(a, 8193n)
    b : Audio <- write(empty, 8192n)
    AudioTest.clock(3, False{})
    c : Audio <- write(b, 0n)
    AudioTest.clock(6, False{})
    d : Audio <- write(c, 0n)
    AudioTest.clock(12, False{})
    e : Audio <- write(d, 0n)
    f : Audio <- write(e, 2n)
    AudioTest.clock(20, False{})
    g : Audio <- write(f, 0n)
    AudioTest.clock(100003, False{})
    h : Audio <- write(g, 2n)
    AudioTest.clock(100010, False{})
    i : Audio <- write(h, 0n)
    AudioTest.clock(0, True{})
    Audio.close(i)

#|0
#|4096
#|4096
#|4096
#|4095
#|4096
#|4095
#|1
#|1
