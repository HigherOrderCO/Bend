# The JS lane's IO.random_u32 called crypto.getRandomValues unguarded, so a
# host whose WebCrypto throws ended the effect and the run instead of
# answering Fail: IO.random_u32 declares Result<&1, &1, U32 & String, U32>
# and the C lane already answers Fail with the errno, so a program that
# handles Fail recovers on C and not on JS. Only a .js twin here, so the
# test pins the JS lane and the C lane, which never failed, is dropped.
import Base

def crypto_broken() -> IO(Unit):
  import "./random_u32_js_fail.js"

def describe(r: Result<&1, &1, U32 & String, U32>) -> String:
  match r:
    case Done{_}:
      "done"
    case Fail{(code, _)}:
      "fail " ++ U32.show(code)

def main() -> IO(Unit):
  do IO<Unit>:
    u : Unit <- crypto_broken()
    r : Result<&1, &1, U32 & String, U32> <- IO.random_u32()
    IO.print(describe(r))

#|fail 5
