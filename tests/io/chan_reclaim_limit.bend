# Dropping each handle permits more channels than the old process-wide limit.
import Base

def churn(n: Nat) -> IO(Unit):
  match n:
    case 0n:
      IO.print("done")
    case 1n+p:
      do IO<Unit>:
        channel : Chan(U32) <- Chan.new(U32, 0)
        churn(p)

def main() -> IO(Unit):
  churn(16777217n)

#|done
