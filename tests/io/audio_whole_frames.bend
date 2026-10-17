# Each write discards its unmatched stereo sample; two halves never join.
# JS only: pause this silent device so timing cannot hide a phantom frame.
import Base

def pause(audio: Audio) -> IO(Audio):
  import "./audio_whole_frames.js"

def report(pair: Audio & U32) -> IO(Unit):
  (audio, queued) = pair
  do IO<Unit>:
    Audio.close(audio)
    IO.print(U32.show(queued))

def main() -> IO(Unit):
  do IO<Unit>:
    a : Audio <- IO.try(Audio, Audio.open(8000))
    b : Audio <- pause(a)
    first : Audio & U32 <- Audio.write(b, [0.0])
    second : Audio & U32 <- Audio.write(Pair.fst(Audio, U32, first), [0.0])
    report(second)

#|0
