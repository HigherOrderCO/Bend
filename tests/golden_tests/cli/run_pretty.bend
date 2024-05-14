main a b =
  switch a {
    0: switch b {
      0: "ba"
      _: "ta"
    }
    _: "ata"
  }
