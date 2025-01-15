# Defining data types

It is possible to easily define complex data types using the `type` keyword.

```py
#{
  A Boolean is either True or False 
}#
type Bool:
  True
  False
```

If a constructor has any arguments, parentheses are necessary around it:
```py
#{
An option either contains some value, or None
}#
type Option:
 Some { value }
 None
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
def Option/map(opt: Option(T), f: T -> U) -> Option(U):
  match opt:
    case Option/Some:
      return f(Option/Some(opt.value))
    case Option/None:
      return Option/None
```

However, they also allow matching on multiple values at once, which is something that regular `match` can't do:

```py
type Boolean:
  True
  False

def Option/is_both_some(lft: Option(T), rgt: Option(T)) -> Boolean:
  match lft:
    case Option/Some:
      match rgt:
        case Option/Some:
          return True
      match rgt:
        case Option/None:
          return False
```

You can read more about pattern matching rules in [Pattern matching](/docs/pattern-matching.md).

In conclusion, the `type` keyword is very useful as it allows you to easily create data types and deconstruct them.
