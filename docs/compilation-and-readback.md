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

Check out [HVM-Core](https://github.com/HigherOrderCO/hvm-core/tree/main#language), one of the Higher Order Company's projects, to know more about this.
