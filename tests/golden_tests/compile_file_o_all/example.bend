# Write definitions like this
(Def1) = (λa a λb b)

# You can call a definition by just referencing its name
# It will be substituted in place of the reference
(Def2) = (λa a Def1)

# Definitions and variables can have names in upper and lower case and contain numbers
# Names defined in a more inner position shadow names in an outer position
(def3) = (λDef1 Def1 λx λx x)

# The language is affine, but if you use a variable more than once the compiler inserts duplications for you
# Of course you can always do them manually
(def4) = λz let {z1 z2} = z; (z1 (λx (x x x x x) z2))

# You can use machine numbers and some native numeric operations
# Numeric operations can only reduce numbers, doing (+ (λx x) 1) will not do anything
(nums) = λx1 λx2 (* (+ x1 1) (/ (- x2 2) 1))

# All files must have a main definition to be run.
# You can execute a program in HVM with "cargo run -- --run <path to file>"
# Other options are "--check" (the default mode) to just see if the file is well formed
# and "--compile" to output hvm-core code.
# (main) = (Def2 1)
(main) = (Def2 1)
