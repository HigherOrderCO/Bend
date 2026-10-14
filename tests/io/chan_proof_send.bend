# an erased proof is null on the JS lane, and a parked receiver used to
# be null too, so a parked send of a proof looked like a receiver and
# every computation waited: a false deadlock the C lane never had
import Base

def producer(
  +data: Chan({0 == 0 : U32}),
  +ready: Chan(Unit)
) -> IO(Unit):
  do IO<Unit>:
    Result<&1, &1, Unit, Unit> <- Chan.send(Unit, ready, Unit{})
    Result<&1, &1, {0 == 0 : U32}, Unit> <- Chan.send({0 == 0 : U32}, data, {==})
    IO.pure(Unit, Unit{})

def run(data: Chan({0 == 0 : U32}), ready: Chan(Unit)) -> IO(Unit):
  +d = data
  +r = ready
  do IO<Unit>:
    Unit <- IO.spawn(Unit, producer(d, r))
    signal : Maybe<&1, Unit> <- Chan.recv(Unit, r)
    proof : Maybe<&1, {0 == 0 : U32}> <- Chan.recv({0 == 0 : U32}, d)
    Unit <- Chan.close(Unit, r)
    Unit <- Chan.close({0 == 0 : U32}, d)
    IO.print("ok")

def with_data(data: Chan({0 == 0 : U32})) -> IO(Unit):
  do IO<Unit>:
    ready : Chan(Unit) <- Chan.new(Unit, 0)
    run(data, ready)

def main() -> IO(Unit):
  do IO<Unit>:
    data : Chan({0 == 0 : U32}) <- Chan.new({0 == 0 : U32}, 0)
    with_data(data)

#|ok
