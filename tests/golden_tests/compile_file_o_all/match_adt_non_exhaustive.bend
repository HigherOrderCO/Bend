type Maybe_ = (Some val) | None

main = @maybe
  match maybe {
    Maybe_/None: 0
    Maybe_/Some: match maybe.val {
      Maybe_/None: 0
    }
  }
