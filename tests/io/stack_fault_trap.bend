# io (io_stack_fault_trap): a machine stack fault is LOUD - the one
# numbered line, then a re-raise of SIGSEGV/SIGBUS. Every host machine
# stack (the event loop's own and each pool worker's) is a guarded
# mmap: a 16KB PROT_NONE page past the window, an altstack, and the
# SIGSEGV/SIGBUS trap prints the memory-fault line before re-raising.
# NOTHING drove it: gutting both sigaction lines kept the whole
# suite green while this program died by raw SIGBUS with an EMPTY
# stderr - the exact silent-loss class the numbered-error law
# forbids. The drive is honest WORK-phase recursion: a fork opens
# the pool, GROW skips the no-fork children, and WORK drains them
# in SEQ mode, where each level of a non-tail self-call pushes a
# real frame; the descent rides a Nat depth (U32.to_nat is O(1):
# Nat is a native immediate) matched ahead of the self-call, and
# 300M levels overrun the 2GB window into the guard page in under
# a second, on whichever lane owns the deep child.
# The JS walls join the numbered-error law at io_run: a cut there
# is a nested host call, so the host stack throws RangeError long
# before the machine's depth, and io_run's catch translates it into
# the C wall's own error 9 line, thrown as the one plain string
# both printers spell verbatim - the interpreter's catch on stdout,
# io_exit's on stderr - exit 1 either way, the compiled-JS deep-cut
# limit now loud AND numbered. The device half of the law (WL_ROOM posting
# ERR_DEEP on the strided device stacks) rides on no host drive, so
# a csrc pin holds it verbatim beside the four host trap sites.
# The print BEFORE the deep call pins the trap's stdout flush: stdout is
# buffered when it goes to a pipe, so a trap that skips the flush loses
# what the program already printed.
import Base

law deep:
  for  n: Nat
  for +a: U32
  for +b: U32
  for +c: U32
  for +d: U32
  U32

def deep(n, a, b, c, d):
  match n:
    case 0n:
      (a + b + c + d : U32)
    case 1n+p:
      x = deep(p, (a + 1 : U32), (b + 2 : U32), (c + 3 : U32), (d + 4 : U32))
      (x + a + b + c + d : U32)

law run:
  for n: U32
  U32

def run(n):
  x y = deep(U32.to_nat(n), 0, 0, 0, 0) deep(U32.to_nat(16), 1, 1, 1, 1)
  (x + y : U32)

law main:
  IO(U32)

def main():
  do IO<U32>:
    a : Unit <- IO.print("BEFORE")
    u : Unit <- IO.print(U32.show(run(300000000)))
    return 0

#|BEFORE
#|bend: memory fault (machine stack overflow?)
#|exit 1
