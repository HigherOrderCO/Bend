# A callback threads a linear array while capturing only scalar values.
# Fusing it must preserve array ownership and the value written at index 1.
import Base

def get(result: Array<U32> & U32, callback: Array<U32> -> U32 -> Array<U32>) -> Array<U32>:
  (array, value) = result
  callback(array, value)

def update(array: Array<U32>, +index: U32, +bias: U32) -> Array<U32>:
  get(Array.get(U32, array, index), a => x =>
    Array.set(U32, a, index, U32.add(x, bias)))

def finish(result: Array<U32> & U32) -> U32:
  (array, value) = result
  value

def main() -> U32:
  a = Array.new(U32, 2n, 10)
  b = update(a, 1, 32)
  finish(Array.get(U32, b, 1))

#|42
