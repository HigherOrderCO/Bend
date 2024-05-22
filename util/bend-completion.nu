def "nu-complete bend opts" [] {
    [all, no-all, eta, no-eta, prune, no-prune, 
    linearize-matches, linearize-matches-alt, 
    no-linearize-matches, float-combinators, 
    no-float-combinators, merge, no-merge, inline, 
    no-inline, check-net-size, no-check-net-size]
}

def "nu-complete bend warn" [] {
    [all, irrefutable-match, redundant-match, unreachable-match, unused-definition, repeated-bind, recursion-cycle]
}

# Checks that the program is syntactically and semantically correct
export extern "bend check" [
    -O: string@"nu-complete bend opts"              # Enables or disables the given optimizations
    --warn(-W): string@"nu-complete bend warn"      # Show the specified compilation warning
    --deny(-D): string@"nu-complete bend warn"      # Deny the specified compilation warning
    --allow(-A): string@"nu-complete bend warn"     # Allow the specified compilation warning
    --help(-h)                                      # Print help
    --entrypoint(-e): string                        # Use other entrypoint rather than main or Main
    --verbose(-v)                                   # Be verbose
]

# Runs the lambda-term level desugaring passes
export extern "bend desugar" [
    -p                                              # Debug and normalization pretty printing
    -O: string@"nu-complete bend opts"              # Enables or disables the given optimizations
    --io                                            # Run with IO enabled
    -l                                              # Linear readback (show explicit dups)
    --stats(-s)                                     # Show runtime stats and rewrite counts
    --warn(-W): string@"nu-complete bend warn"      # Show the specified compilation warning
    --deny(-D): string@"nu-complete bend warn"      # Deny the specified compilation warning
    --allow(-A): string@"nu-complete bend warn"     # Allow the specified compilation warning
    --help(-h)                                      # Print help
    --entrypoint(-e): string                        # Use other entrypoint rather than main or Main
    --verbose(-v)                                   # Be verbose
]

# Compiles the program to hvmc and prints to stdout
export extern "bend gen-hvm" [
    -p                                              # Debug and normalization pretty printing
    -O: string@"nu-complete bend opts"              # Enables or disables the given optimizations
    --io                                            # Run with IO enabled
    -l                                              # Linear readback (show explicit dups)
    --stats(-s)                                     # Show runtime stats and rewrite counts
    --warn(-W): string@"nu-complete bend warn"      # Show the specified compilation warning
    --deny(-D): string@"nu-complete bend warn"      # Deny the specified compilation warning
    --allow(-A): string@"nu-complete bend warn"     # Allow the specified compilation warning
    --help(-h)                                      # Print help
    --entrypoint(-e): string                        # Use other entrypoint rather than main or Main
    --verbose(-v)                                   # Be verbose
]

# Compiles the program to standalone C and prints to stdout
export extern "bend gen-c" [
    -p                                              # Debug and normalization pretty printing
    -O: string@"nu-complete bend opts"              # Enables or disables the given optimizations
    --io                                            # Run with IO enabled
    -l                                              # Linear readback (show explicit dups)
    --stats(-s)                                     # Show runtime stats and rewrite counts
    --warn(-W): string@"nu-complete bend warn"      # Show the specified compilation warning
    --deny(-D): string@"nu-complete bend warn"      # Deny the specified compilation warning
    --allow(-A): string@"nu-complete bend warn"     # Allow the specified compilation warning
    --help(-h)                                      # Print help
    --entrypoint(-e): string                        # Use other entrypoint rather than main or Main
    --verbose(-v)                                   # Be verbose
]

# Compiles the program to standalone Cuda and prints to stdout
export extern "bend gen-cu" [
    -p                                              # Debug and normalization pretty printing
    -O: string@"nu-complete bend opts"              # Enables or disables the given optimizations
    --io                                            # Run with IO enabled
    -l                                              # Linear readback (show explicit dups)
    --stats(-s)                                     # Show runtime stats and rewrite counts
    --warn(-W): string@"nu-complete bend warn"      # Show the specified compilation warning
    --deny(-D): string@"nu-complete bend warn"      # Deny the specified compilation warning
    --allow(-A): string@"nu-complete bend warn"     # Allow the specified compilation warning
    --help(-h)                                      # Print help
    --entrypoint(-e): string                        # Use other entrypoint rather than main or Main
    --verbose(-v)                                   # Be verbose
]

# Compiles the program and runs it with the Rust HVM implementation
export extern "bend run" [
    -p                                              # Debug and normalization pretty printing
    -O: string@"nu-complete bend opts"              # Enables or disables the given optimizations
    --io                                            # Run with IO enabled
    -l                                              # Linear readback (show explicit dups)
    --stats(-s)                                     # Show runtime stats and rewrite counts
    --warn(-W): string@"nu-complete bend warn"      # Show the specified compilation warning
    --deny(-D): string@"nu-complete bend warn"      # Deny the specified compilation warning
    --allow(-A): string@"nu-complete bend warn"     # Allow the specified compilation warning
    --help(-h)                                      # Print help
    --entrypoint(-e): string                        # Use other entrypoint rather than main or Main
    --verbose(-v)                                   # Be verbose
]

# Compiles the program and runs it with the C HVM implementation
export extern "bend run-c" [
    -p                                              # Debug and normalization pretty printing
    -O: string@"nu-complete bend opts"              # Enables or disables the given optimizations
    --io                                            # Run with IO enabled
    -l                                              # Linear readback (show explicit dups)
    --stats(-s)                                     # Show runtime stats and rewrite counts
    --warn(-W): string@"nu-complete bend warn"      # Show the specified compilation warning
    --deny(-D): string@"nu-complete bend warn"      # Deny the specified compilation warning
    --allow(-A): string@"nu-complete bend warn"     # Allow the specified compilation warning
    --help(-h)                                      # Print help
    --entrypoint(-e): string                        # Use other entrypoint rather than main or Main
    --verbose(-v)                                   # Be verbose
]

# Compiles the program and runs it with the Cuda HVM implementation
export extern "bend run-cu" [
    -p                                              # Debug and normalization pretty printing
    -O: string@"nu-complete bend opts"              # Enables or disables the given optimizations
    --io                                            # Run with IO enabled
    -l                                              # Linear readback (show explicit dups)
    --stats(-s)                                     # Show runtime stats and rewrite counts
    --warn(-W): string@"nu-complete bend warn"      # Show the specified compilation warning
    --deny(-D): string@"nu-complete bend warn"      # Deny the specified compilation warning
    --allow(-A): string@"nu-complete bend warn"     # Allow the specified compilation warning
    --help(-h)                                      # Print help
    --entrypoint(-e): string                        # Use other entrypoint rather than main or Main
    --verbose(-v)                                   # Be verbose
]

# A high-level, massively parallel programming language
export extern "bend help" [
    --help(-h)                  # Print help
    --version(-V)               # Print version
    --entrypoint(-e): string    # Use other entrypoint rather than main or Main
    --verbose(-v)               # Be verbose
]

# A high-level, massively parallel programming language
export extern "bend" [
    --help(-h)                  # Print help
    --version(-V)               # Print version
    --entrypoint(-e): string    # Use other entrypoint rather than main or Main
    --verbose(-v)               # Be verbose
]