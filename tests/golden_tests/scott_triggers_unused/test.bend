# The test below should not trigger this warning:
#
# In definition 'f':
#   Definition is unused.
# In definition 't':
#   Definition is unused.
#
# This was happening because the prune algorithm was just collecting constructors
# by searching for tags.

data bool = t | f

main = @b match b { bool/t: 0; bool/f: 1}
