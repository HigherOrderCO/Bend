data Option = (Some val) | None

Option/or = @a @b match a {
  Option/Some: a
  Option/None: b
}

main = (Option/or (Option/Some 5) (Option/None))
