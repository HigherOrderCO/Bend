# Helpers for the rules in this folder that are written in Bend.
import Base
import ../src/bend/lint.bend as Lint

def root_or(root: Bool, s: Lint.Source, later: Maybe<&2, Lint.Source>) -> Maybe<&2, Lint.Source>:
  match root:
    case True{}:
      Some{s}
    case False{}:
      later

def is_root(s: Lint.Source) -> Bool:
  match s:
    case Lint.Source{path, text, root}:
      root

# The source of the file linted.
def root(srcs: List<&2, Lint.Source>) -> Maybe<&2, Lint.Source>:
  match srcs:
    case Nil{}:
      None{}
    case Con{+s, rest}:
      root_or(is_root(s), s, root(rest))

# Count physical lines in one tail-recursive scan, without allocating a
# line list. An empty file has zero lines; a final newline adds no sentinel.
def line_count.go(s: String, count: U32, last: Bool) -> U32:
  match s:
    case SNil{}:
      match last:
        case True{}:
          count
        case False{}:
          U32.add(count, 1)
    case SCon{c, rest}:
      match c:
        case '\n':
          line_count.go(rest, U32.add(count, 1), True{})
        case _:
          line_count.go(rest, count, False{})

def line_count(text: String) -> U32:
  line_count.go(text, 0, True{})
