# A backward wall clock must not add frames to the silent audio ring.
# JS only: the native ring drains by the device, without a wall clock.
import Base

def wall_clock_backwards(reset: Bool) -> IO(Unit):
  import "./audio_monotonic_clock.js"

def report(pair: Audio & U32) -> IO(Unit):
  (audio, queued) = pair
  do IO<Unit>:
    Unit <- Audio.close(audio)
    IO.print(U32.show(queued))

def main() -> IO(Unit):
  do IO<Unit>:
    Unit <- wall_clock_backwards(False{})
    a : Audio <- IO.try(Audio, Audio.open(48000))
    pair : Audio & U32 <- Audio.write(a, [])
    Unit <- wall_clock_backwards(True{})
    report(pair)

#|0
