type Option = (Some val) | None

Option/and = @a @b match a {
  Option/Some: match b {
    Option/Some: (Option/Some (a.val, b.val))
    Option/None: Option/None
  }
  Option/None: Option/None
}

main = (Option/and (Option/Some 1) (Option/Some 3))
