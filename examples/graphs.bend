##############################
# Author: Ematth, 2024
##############################

### Graph Definition: ###

object Node { value }
object Edge { weight, node1, node2 }
object Pair { x, y }

type Graph:
  Pair 
  Nil
  
  

#########################

# REMOVE WHEN IMPORTING GETS ADDED
# List/len = @l
#   match l {
#     List/Nil: 0
#     List/Cons: (+ 1 (List/len l.tail))
#   }

# # REMOVE WHEN IMPORTING GETS ADDED
# List/sum = @l
#   match l {
#     List/Cons: (+ l.head (List/sum l.tail))
#     List/Nil: 0
#   }

# # Graph: count nodes
# # Graph g -> uint
# def Graph/count_nodes(g):
#   open Graph: g
#   return List/len(g.nodes)

# # Graph: count edges
# # Graph g -> uint
# def Graph/count_edges(g):
#   open Graph: g
#   return List/len(g.edges)

# def Graph/sum_nodes(g):
#   open Graph: g
#   match g.nodes:
#     case List/Cons: 
#       return List/sum(g.nodes)
#     case List/Nil: 
#       return 0


# ###########################################

def main:

  # (a, b, c) = [Node(1), Node(2), Node(3)]
  # nodes = [a, b, c]
  # edges = [Edge(3, a, b), Edge(7, b, c), Edge(5, a, c)]

  # g = Graph(nodes, edges)
  a = Pair(3, 5)
  open Pair: a
  return (a.x, a.y)
