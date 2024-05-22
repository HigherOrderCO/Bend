type Option = (Some x) | (None)

some_some (Option/Some (Option/Some x)) = 1
some_some *                             = 0

main = (some_some (Option/Some 1))
