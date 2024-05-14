data Option = (Some val) | None

main = @a @b match a {
  Option/Some: match b {
    Option/Some: (Option/Some (a.val, b.val))
    Option/None: Option/None
  }
  Option/None: Option/None
}
