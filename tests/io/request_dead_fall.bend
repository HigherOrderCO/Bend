# io (io_request_dead_fall): a main that IS one effect call, no
# IO.bind (every bench main has this shape), runs through the one
# request protocol: the event loop applies main's closure to the Emit
# continuation through FID_Clo_apply, the foreign REQUEST segment
# builds the request node, and the loop runs it. No shortcut reads a
# foreign closure's captures in the loop, so FID_Clo_apply, FID_IO_emit
# and the reached foreign segments are live in every program.
import Base

law main:
  IO(Unit)

def main():
  IO.print("dead fall")

#|dead fall
