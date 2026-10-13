# io (io_select_eintr): a signal during the kernel wait wakes no idle fd.
# Idle.park installs a no-op handler and parks on an unwritten pipe;
# main sleeps 30 ms and exits while that park is still live.
# The original JS select bug treated unchanged fd sets after EINTR as
# readiness, so an unfinished TCP.connect falsely answered Done.
# An interrupted poller reports no fd readiness; due timers still run.
import Base

def Idle.park(ms: U32) -> IO(Unit):
  import "./select_eintr.c"
  import "./select_eintr.js"

def idle() -> IO(Unit):
  do IO<Unit>:
    Unit <- Idle.park(5)
    IO.print("the idle park woke")

def main() -> IO(U32):
  do IO<U32>:
    Unit <- IO.spawn(Unit, idle())
    Unit <- IO.sleep(30)
    Unit <- IO.print("slept")
    IO.die(U32, 0, "")

#|slept
