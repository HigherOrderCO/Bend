# Real settled, timer-backed, rejected and WebCrypto host promises.
import Base

def Host.resolved() -> IO(U32):
  import "./host_promise.js"

def Host.timer() -> IO(U32):
  import "./host_promise.js"

def Host.rejected() -> IO(U32):
  import "./host_promise.js"

def Host.digest() -> IO(String):
  import "./host_promise.js"

def main() -> IO(Unit):
  do IO<Unit>:
    a : U32 <- Host.resolved()
    b : U32 <- Host.timer()
    c : U32 <- Host.rejected()
    Unit <- IO.print(U32.show((a + (b + c : U32) : U32)))
    digest : String <- Host.digest()
    IO.print(digest)

#|31
#|ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad
