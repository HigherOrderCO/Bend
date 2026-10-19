# io pin (io_tls_connect_close): bend3's IO/TLS minted tls_-prefixed
# effect constructors and forwarded TCP-shaped aliases to them. bend
# has no TLS surface (only the bare Socket handle type) -- the do
# block dies on the missing surface.
import Base

law main:
  IO(U32)

def main():
  do IO<U32>:
    h : Socket <- IO.try(Socket, TLS.connect("example.com", 443))
    x : Unit <- TLS.close(h)
    return 0

#|SOME PROOFS FAIL
#|Error:
#|- expected : a defined name
#|- observed : TLS.connect
#|Location: main
#|11 |   do IO<U32>:
#|12>|     h : Socket <- IO.try(Socket, TLS.connect("example.com", 443))
#|   |                                  ^^^^^^^^^^^
#|13 |     x : Unit <- TLS.close(h)
#|Note: did you mean TCP.connect?
#|exit 1
