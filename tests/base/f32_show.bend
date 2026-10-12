# F32.show fits the same plain formatter slot as the other Base shows,
# directly in List.show and Maybe.show, without a lambda wrapper.
import Base

def main() -> IO(Unit):
  do IO<Unit>:
    IO.print(List.show(~&2, ~F32, ~F32.show, [1.5, 2.5]))
    IO.print(Maybe.show(~&2, ~F32, ~F32.show, Some{1.5}))

#|[1.5, 2.5]
#|Some(1.5)
