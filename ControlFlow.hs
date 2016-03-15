module ControlFlow where
import Data.Tuple

-- Types for labelling and control flow analysis of
-- any type representing an AST (parametric type "statement")

type Label = Int
type Edge = (Label,Label)

type Node statement = (Label,statement)
type ControlFlowGraph statement = ([Node statement],[Edge])

reverseFlow :: [Edge] -> [Edge]
reverseFlow edges = map swap edges

finalBlocks :: ControlFlowGraph s-> [Label]
finalBlocks (nodes,edges) = filter (not . hasOutgoingEdge) $ map fst nodes
  where hasOutgoingEdge node = any (\(l1,_) -> l1 == node) edges

initialBlocks :: ControlFlowGraph s -> [Label]
initialBlocks (nodes,edges) = filter (not . hasIncomingEdge) $ map fst nodes
  where hasIncomingEdge node = any (\(_,l2) -> l2 == node) edges



