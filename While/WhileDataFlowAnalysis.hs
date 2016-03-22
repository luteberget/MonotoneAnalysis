module WhileDataFlowAnalysis where

import MonotoneFrameworkAnalysis
import ControlFlow
import WhileAST
import WhileParser
import Text.Parsec (parse)
import System.IO
import System.Environment
import Data.List

-- Main function which uses an MF analysis configuration record
-- to create an analysis program

dataFlowAnalysisMain :: (Show domain) => String -> (Program -> [(Label, [domain])]) -> IO ()
dataFlowAnalysisMain title analysis = do
  putStrLn ("While language analysis: " ++ title)
  (fn:_) <- getArgs
  withFile fn ReadMode (\f -> do
    contents <- hGetContents f
    let p = parse WhileParser.programParser fn contents
    let program = case p of Left err -> error (show err)
                            Right s -> s
    putStrLn "Parsed program: "
    putStrLn (show program)
    putStrLn (title ++ ": ")
    putStrLn (show (analysis program))
    )
  putStrLn "End."

  -- Following are various functions for extracting relevant information
  -- from the AST about variables and expressions for the analysis types
  -- using the data flow main method.

programVariables :: Program -> [Variable]
programVariables p = nub (assignmentVariables p)

assignmentVariables :: Statement -> [Variable]
assignmentVariables Skip = []
assignmentVariables (Assignment v _) = [v]
assignmentVariables (Sequential s1 s2) = assignmentVariables s1 ++ assignmentVariables s2
assignmentVariables (IfThenElse _ s1 s2) = assignmentVariables s1 ++ assignmentVariables s2
assignmentVariables (While _ b) = assignmentVariables b

nonTrivialArithSubExprs :: ArithExpr -> [ArithExpr]
nonTrivialArithSubExprs (Constant _) = []
nonTrivialArithSubExprs (Variable _) = []
nonTrivialArithSubExprs e@(ArithBinOpExpr e1 op e2) = [e] ++ nonTrivialArithSubExprs e1 ++ nonTrivialArithSubExprs e2

statementArithSubExprs :: Statement -> [ArithExpr]
statementArithSubExprs Skip = []
statementArithSubExprs (Assignment _ e) = nonTrivialArithSubExprs e
statementArithSubExprs (Sequential s1 s2) = statementArithSubExprs s1 ++ statementArithSubExprs s2
statementArithSubExprs (IfThenElse c s1 s2) = boolExprArithSubExprs c ++ statementArithSubExprs s1 ++ statementArithSubExprs s2
statementArithSubExprs (While c b) = boolExprArithSubExprs c ++ statementArithSubExprs b

boolExprArithSubExprs :: BoolExpr -> [ArithExpr]
boolExprArithSubExprs (BoolBinOpExpr e1 _ e2) = boolExprArithSubExprs e1 ++ boolExprArithSubExprs e2
boolExprArithSubExprs (ArithBoolOpExpr e1 _ e2) = nonTrivialArithSubExprs e1 ++ nonTrivialArithSubExprs e2
boolExprArithSubExprs _ = []

containsVariable :: ArithExpr -> Variable -> Bool
containsVariable (Constant _) _ = Prelude.False
containsVariable (Variable v1) v2 = v1 == v2
containsVariable (ArithBinOpExpr e1 _ e2) v = (containsVariable e1 v) || (containsVariable e2 v)

expressionVariables :: ArithExpr -> [Variable]
expressionVariables (Constant _) = []
expressionVariables (Variable v) = [v]
expressionVariables (ArithBinOpExpr e1 _ e2) = expressionVariables e1 ++ expressionVariables e2

boolExprVariables :: BoolExpr -> [Variable]
boolExprVariables (BoolBinOpExpr e1 _ e2) = boolExprVariables e1 ++ boolExprVariables e2
boolExprVariables (ArithBoolOpExpr e1 _ e2) = expressionVariables e1 ++ expressionVariables e2
boolExprVariables _ = []
