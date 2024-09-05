# Eta-reduction of some recursive functions can mess the performance
# of programs targeting the CUDA runtime.

# So for now we've disabled it by default when the target architecture
# is not C (for which eta-reduction usually seems to not only decrease
# the total amount of interactions, but actually slightly increase 
# performance a little bit is the tested cases).

# This test checks that the compiler doesn't eta-reduce when calling
# 'gen-hvm', which has an unknown target architecture.

# A recursive function
make_list n x = switch n {
  0: @l l
  _: @l (l x (make_list n-1 x))
}

# A non-recursive function
make_10 x = (make_list 10 x)

main = (make_10 1)