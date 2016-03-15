import WhileAST
import ControlFlow
import MonotoneFrameworkAnalysis
import WhileControlFlowAnalysis
import WhileDataFlowAnalysis
import Data.List
import Data.Tuple

killVB :: Program -> Statement -> [ArithExpr]
killVB p (Assignment v _) = [ a | a <- programExprs, containsVariable a v]
  where programExprs = nub (statementArithSubExprs p)
killVB _ _ = []

genVB :: Statement -> [ArithExpr]
genVB (IfThenElse c _ _ ) = nub $ boolExprArithSubExprs c
genVB (While c _ ) = nub $ boolExprArithSubExprs c
genVB (Assignment _ e) = nub $ nonTrivialArithSubExprs e
genVB _ = []

fl :: Program -> (Label,Statement) -> [ArithExpr] -> [ArithExpr]
fl p (_,s) es = union (es \\ (killVB p s)) (genVB s)

veryBusyExpressions :: Program -> [(Label,[ArithExpr])]
veryBusyExpressions program = monotoneFrameworkAnalysis conf
  where conf = MFInstance {
          partialOrder = superset,
          leastUpperBound = intersect,
          leastElement = nub (statementArithSubExprs program),
          initialElement = [],
          extremalLabels = finalBlocks cfa,
          flow = reverseFlow edges,
          labeledProgram = nodes,
          transformation = fl program,
          output = PreTransform
          }
        cfa@(nodes,edges) = controlFlowAnalysis program

main :: IO ()
main = dataFlowAnalysisMain "Very busy expressions" veryBusyExpressions
