main = @maybe
  match maybe {
    Maybe/None: 0
    Maybe/Some: match maybe.value {
      Maybe/None: 0
    }
  }
