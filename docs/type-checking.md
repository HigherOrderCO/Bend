# Type Checking

The Bend type checker is implemented based on a gradual type system with Hindley Milner inference.

Programs can be optionally typed using the respective imp or fun type syntax. Type checking is
enabled by default, and can also be specified with the `-Otype-check` option.

Bend has some builtin types such as `i24`, `u24`, `f24`, Integer and Number. Although Integer and
Number cannot be used directly.

In example, the program below will not compile or run since some of the list elements do not have
type `i24`.

```python
type Option(T):
  Some { value: T }
  None

def head(list: List(T)) -> Option(T):
  match list:
    case List/Cons:
      return Option/Some { value: list.head }
    case List/Nil:
      return Option/None

def main -> Option(i24):
  return head([1 ,"a"])
```

In some cases we know that dynamically our program will not do something wrong.

To disable type checking we can remove the type from some function or use the `-Ono-type-check`
option to not type check the entire program:

```python
def main:
  return head([1 ,"a"])
```

## Limitations

Currently, it is not possible to:

- Define or check a superposed type or term.
- Check unscoped variables.
- ~~Check tagged terms.~~

In example, we know that it could be possible to call `add` with a superposed term and have `{3 4}`
as result.

```python
def add(x: _, y: _) -> _:
  return x + y

def main -> _:
  return add(1, {2, 3})
```
