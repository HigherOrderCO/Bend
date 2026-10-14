# The Linux audio pump against injected ALSA write results (ALSA's null
# output underneath, so no device). Other systems run no cases.
import Base

def AudioTest.count() -> IO(U32):
  import "./audio_linux_progress.c"
  import "./audio_linux_progress.js"

def AudioTest.start(i: U32) -> IO(Unit):
  import "./audio_linux_progress.c"
  import "./audio_linux_progress.js"

def AudioTest.wait() -> IO(Unit):
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

def cases(n: Nat, +i: U32) -> IO(Unit):
  match n:
    case 0n:
      IO.print("audio")
    case 1n+p:
      do IO<Unit>:
        Unit <- AudioTest.start(i)
        a : Audio <- IO.try(Audio, Audio.open(48000))
        pair : Audio & U32 <- Audio.write(a, samples(512n, 1))
        Unit <- AudioTest.wait()
        Unit <- close_pair(pair)
        Unit <- AudioTest.closed()
        cases(p, (i + 1 : U32))

def main() -> IO(Unit):
  do IO<Unit>:
    n : U32 <- AudioTest.count()
    cases(U32.to_nat(n), 0)

#|audio
