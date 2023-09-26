Readback:
  Support more cases without giving invalid readback.
  Show global lambdas.
  Differentiate between bad readback in hvm-core (like if there's a cyclic net) and bad readback in hvm-lang (like a closed subnet on a lambda variable).
  Put dups in the correct scope.
  Find a solution (in terms of syntax) for what to do when dups cannot be scoped (eg. dup a b = c; dup c d = a; ... This is cyclic but there are probably valid examples).

Pattern matching:
  Infer type of definitions.
  Check that types are used consistently and constructor names are unique.
  Convert pattern matching rules and constructors inside terms into lambda calculus.

Optimizations:
  Find a good heuristic for when to extract combinators in subterms (and maybe inline small ones).
  Merge identical definitions
  Unused definition pruning.
  Constant folding and prereduction of rules.
  Consider different encoding for pattern matching.
  Pattern matching without converting into lambda calculus (lambda calculus with patterns directly into inets).

Parser:
  Consider using chumsky also for the lexer.
  Return spanned values.

Error reporting:
  Use error types and not just fail with opaque anyhow::Error.
  Fix span of errors.
  Add nice looking errors for errors outside the parser.
  Improve error messages.

Convert from lambda terms directly into LNet without using the intermediate vector of nodes form
Numbers and numerical operations (need hvm support)
Documentation
Benchmarking the compiler

