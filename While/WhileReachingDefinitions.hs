import WhileAST
import ControlFlow
import WhileControlFlowAnalysis
import WhileDataFlowAnalysis
import MonotoneFrameworkAnalysis
import Data.List
import Debug.Trace

type ReachFrom = (Label,Variable)

killFilter :: Statement -> [ReachFrom] -> [ReachFrom]
killFilter (Assignment variable _) lst = filter (\reach -> (snd reach) /= variable) lst
killFilter _ lst = lst

gen :: (Label, Statement) -> [ReachFrom]
gen (label,(Assignment v _)) = [(label,v)]
gen _ = []

fl :: (Label,Statement) -> [ReachFrom] -> [ReachFrom]
fl (label,statement) existing = (union . nub) (killFilter statement existing) (gen (label,statement))

reachingDefinitions :: Program -> [(Label,[ReachFrom])]
reachingDefinitions program = monotoneFrameworkAnalysis conf
  where conf = MFInstance {
          partialOrder = subset,
          leastUpperBound = union . nub,
          leastElement = [],
          initialElement = map (\v -> (-1,v)) (programVariables program),
          extremalLabels = initialBlocks cfg,
          flow = edges,
          labeledProgram = nodes,
          transformation = fl,
          output = PreTransform
          }
        cfg@(nodes,edges) = controlFlowAnalysis program

main :: IO ()
main = dataFlowAnalysisMain "Reaching definitions" reachingDefinitions
