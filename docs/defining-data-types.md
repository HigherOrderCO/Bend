# Defining data types

It is possible to easily define complex data types using the `type` keyword.

```py
# A Boolean is either True or False 
type Bool:
  True
  False
```

If a constructor has any arguments, parentheses are necessary around it:
```py
# An option either contains some value, or None
type Option:
 Some { value }
 None
```
If the data type has a single constructor, it can be destructured using `open`:

```py
# A Box is a wrapper around a value.
type Boxed:
  Box { value }

def main() -> _:
  b = Boxed/Box(1)
  open Boxed: b
  return b.value
```


The fields of the constructor that is being destructured with the `match` are bound to the matched variable plus `.` and the field names.
```py
opt = Option/Some(1)
match opt:
  case Option/Some:
    return opt.value
  case Option/None:
    return 0
```

Rules can also have patterns.
They work like match expressions with explicit bindings:

```py
(Option.map (Some value) f) = (Some (f value))
(Option.map None f) = None
```

However, they also allow matching on multiple values at once, which is something that regular `match` can't do:

```py
type Boolean:
  True
  False

(Option.is_both_some (Some lft_val) (Some rgt_val)) = True
(Option.is_both_some lft rgt) = False
```

You can read more about pattern matching rules in [Pattern matching](/docs/pattern-matching.md).

In conclusion, the `type` keyword is very useful as it allows you to easily create data types and deconstruct them.
