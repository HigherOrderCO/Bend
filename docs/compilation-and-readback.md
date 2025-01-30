# Compilation and readback

How are terms compiled to interaction net nodes?

HVM has a bunch of useful nodes to write IC programs.
Every node contains one `main` port `0` and two `auxiliary` ports, `1` and `2`.

There are 7 kinds of nodes, Eraser, Constructor, Duplicator, Reference, Number, Operation and Match.

A lambda `λx x` compiles into a Constructor node.
An application `((λx x) (λx x))` also compiles into a Constructor node.

```
  0 - Points to the lambda occurrence      0 - Points to the function
  |                                        |
  λ Lambda                                 @ Application
 / \                                      / \
1   2 - Points to the lambda body        1   2 - Points to the application occurrence
|                                        |
Points to the lambda variable            Points to the argument
```

When reading back, if we visit a Constructor via port 0 then we know it's a lambda, and if we visit it via port 2 it's an application.

- The `Number` node uses the label to store it's number.
- An `Operation` node uses the label to store it's operation.

A duplication `let {a b} = x` compiles into a Duplicator node.
A superposition `{a b}` compiles to a Duplicator node too. The difference here comes from context too.

```
  0 - Points to the sup occurrence         0 - Points to the duplicated value
  |                                        |
  # Superposition                          # Duplication
 / \                                      / \
1   2 - Points to the second value       1   2 - Points to the second binding
|                                        |
Points to the first value                Points to the first binding
```
Bend core terms directly compile to HVM nodes.
- Application -> CON node with --+ polarization.
- Lambda -> CON node with ++- polarization.
- Duplication -> DUP node with -++ polarization.
- Superposition -> DUP node with +-- polarization
- Pairs -> CON node with +-- polarization.
- Pair elimination -> CON node with -++ polarization.
- Erasure values (as in λx *) -> ERA node with + polarization.
- Erased variables (as in λ* x) -> ERA node with + polarization.
- Numbers -> NUM node (always + polarization).
- Switches -> MAT node (--+) + CON node (+--) on port 1 that links to the if 0 and if >= 1 cases.
- Numeric operations -> an OPR node (--+) plus a NUM that holds the kind of operation as per the HVM2 paper.
- References to top level functions -> REF node (+).

Matches get compiled to the above core constructs according to the adt-encoding option.
Check out [HVM2](https://github.com/HigherOrderCO/HVM), one of the Higher Order Company's projects, to know more about this.

### Bend compiler passes:

**encode_adt**: Create functions for constructors.  
**desugar_open**: Convert open terms into match terms.  
**encode_builtins**: Convert sugars for builtin types (e.g., list, string) into function calls.  
**desugar_match_def**: Convert equational-style pattern matching functions into trees of match and switch terms.  
**fix_match_terms**: Normalize all match and switch terms.  
**lift_local_defs**: Convert `def` terms into top-level functions.  
**desugar_bend**: Convert Bend terms into top-level functions.  
**desugar_fold**: Convert `fold` terms into top-level functions.  
**desugar_with_blocks**: Convert `with` terms and ask (`<-`) terms into monadic bind and unit (wrap).  
**make_var_names_unique**: Give a unique name to each variable in each function.  
**desugar_use**: Resolve alias terms (`use`) by substituting their occurrences with the aliased term (syntactic duplication).  
**linearize_matches**: Linearize the variables in match and switch terms according to the linearize-matches option.  
**linearize_match_with**: Linearize the variables specified in `with` clauses of match and switch if they haven't already been linearized by `linearize_matches`.  
**type_check_book**: Run the type checker (no elaboration, only inference/checking).  
**encode_matches**: Transform match terms into their lambda-calculus forms as specified by the adt-encoding option.  
**linearize_vars**: Linearize the occurrences of variables by placing duplicates when variables are used more than once, erasing unused variables, and inlining `let` terms whose variables only occur once.  
**float_combinators**: Convert combinator terms into top-level functions according to the size heuristic described in the source code.  
**prune**: Remove unused functions according to the prune option.  
**merge_definitions**: Merge identical top-level functions.  
**expand_main**: Expand the term of the `main` function by dereferencing so that it includes computation and isn't just lazy refs or data containing lazy refs.  
**book_to_hvm**: Lower to HVM (as described in the compilation-and-readback file).  
**eta**: Perform eta-reduction at the inet level without reducing nodes with `ERA` or `NUM` at both ports (logically equivalent but looks incorrect to users).  
**check_cycles**: Heuristic check for mutually recursive cycles of function calls that could cause loops in HVM.  
**inline_hvm_book**: Inline REFs to networks that are nullary nodes.  
**prune_hvm_book**: Additional layer of pruning after eta-reducing at the inet level.  
**check_net_sizes**: Ensure no generated definition will be too large to run on the CUDA runtime.  
**add_recursive_priority**: Mark some binary recursive calls with a flag at the inet level so that the GPU runtime can properly distribute work.

