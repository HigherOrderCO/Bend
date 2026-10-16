# a Base-free module's && and < are its own Bool.and and W.is_lt, even
# nested under && (an annotation reaches the operators inside it)
type Bool is Data:
  True{}
  False{}

type W is Data:
  W{}

def W.is_lt(a: W, b: W) -> Bool:
  True{}

def Bool.and(a: Bool, b: Bool) -> Bool:
  a

def t(a: W, b: W, c: W, d: W) -> Bool:
  (a < b && c < d : W)

def main() -> Bool:
  t(W{}, W{}, W{}, W{})

#|True{}
