# Host getters count predicate calls in exported list searches.
import Base

def Search.calls() -> IO(U32):
  import "./list_search_calls.js"

def main() -> IO(Unit):
  do IO<Unit>:
    result : U32 <- Search.calls()
    IO.print(U32.show(result))

#|0
