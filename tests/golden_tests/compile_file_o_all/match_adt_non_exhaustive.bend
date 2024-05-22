type Maybe = (Some val) | None

main = @maybe
  match maybe {
    Maybe/None: 0
    Maybe/Some: match maybe.val {
      Maybe/None: 0
    }
  }
