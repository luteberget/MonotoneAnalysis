import WhileAST
import ControlFlow
import MonotoneFrameworkAnalysis
import WhileControlFlowAnalysis
import WhileDataFlowAnalysis
import Data.List

killAE :: Program -> Statement -> [ArithExpr]
killAE p (Assignment v _) = [ a | a <- programExprs, containsVariable a v]
  where programExprs = nub (statementArithSubExprs p)
killAE _ _ = []

genAE :: Statement -> [ArithExpr]
genAE (IfThenElse c _ _ ) = nub $ boolExprArithSubExprs c
genAE (While c _ ) = nub $ boolExprArithSubExprs c
genAE (Assignment v e) = [ a | a <- nub $ nonTrivialArithSubExprs e, not (containsVariable a v)]
genAE _ = []

fl :: Program -> (Label,Statement) -> [ArithExpr] -> [ArithExpr]
fl p (_,s) es = union (es \\ (killAE p s)) (genAE s)

availableExpressions :: Program -> [(Label,[ArithExpr])]
availableExpressions program = monotoneFrameworkAnalysis conf
  where conf = MFInstance {
          partialOrder = superset,
          leastUpperBound = intersect.nub,
          leastElement = nub (statementArithSubExprs program),
          initialElement = [],
          extremalLabels = initialBlocks cfg,
          flow = edges,
          labeledProgram = nodes,
          transformation = fl program,
          output = PreTransform
          }
        cfg@(nodes,edges) = controlFlowAnalysis program

main :: IO ()
main = dataFlowAnalysisMain "Available expressions" availableExpressions
