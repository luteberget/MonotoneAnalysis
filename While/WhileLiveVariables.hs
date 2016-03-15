import WhileAST
import ControlFlow
import WhileControlFlowAnalysis
import WhileDataFlowAnalysis
import MonotoneFrameworkAnalysis
import Data.List

kill :: Statement -> [Variable]
kill (Assignment v _) = [v]
kill _ = []

gen :: Statement -> [Variable]
gen (IfThenElse c _ _) = nub $ boolExprVariables c
gen (While c _) = nub $ boolExprVariables c
gen (Assignment _ e) = nub $ expressionVariables e
gen _ = []

fl :: (Label,Statement) -> [Variable] -> [Variable]
fl (_,statement) existing = union (existing \\ (kill statement)) (gen statement)

liveVariables :: Program -> [(Label,[Variable])]
liveVariables program = monotoneFrameworkAnalysis conf
  where conf = MFInstance {
          partialOrder = subset,
          leastUpperBound = union.nub,
          leastElement = [],
          initialElement = [],
          extremalLabels = finalBlocks cfg,
          flow = reverseFlow edges,
          labeledProgram = nodes,
          transformation = fl,
          output = PostTransform
          }
        cfg@(nodes,edges) = controlFlowAnalysis program

main :: IO ()
main = dataFlowAnalysisMain "Live variables" liveVariables
