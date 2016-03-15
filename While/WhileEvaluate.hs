module WhileEvaluate where

import WhileAST
import Data.Function
import Data.List
type SymbolTable = [(String,Integer)]

-- Evaluation of WHILE expressions and statements
-- Corresponds to structural operational semantics given
-- in Table 2.6 of /Principles of Program Analysis/.

evalArithExpr :: ArithExpr -> SymbolTable -> Integer
evalArithExpr (Constant c) _ = c
evalArithExpr (Variable x) t = case lookup x t of
  Nothing -> error ("unbound variable " ++ x ++ ".")
  Just v -> v
evalArithExpr (ArithBinOpExpr ae1 op ae2) t
  | op == Plus = e (+)
  | op == Minus = e (-)
  | op == Mul = e (*)
  | op == Div = e (div)
  where e o = (o (evalArithExpr ae1 t) (evalArithExpr ae2 t))

evalBoolExpr :: BoolExpr -> SymbolTable -> Bool
evalBoolExpr WhileAST.True _ = Prelude.True
evalBoolExpr WhileAST.False _ = Prelude.False
evalBoolExpr (Not expr) t = not (evalBoolExpr expr t)
evalBoolExpr (BoolBinOpExpr be1 op be2) t
  | op == BoolEq = e (==)
  where e o = (o (evalBoolExpr be1 t) (evalBoolExpr be2 t))
evalBoolExpr (ArithBoolOpExpr ae1 op ae2) t
  | op == Lt = e (<)
  | op == Lte = e (<=)
  | op == Gt = e (>)
  | op == Gte = e (<=)
  | op == ArithEq = e (==)
  where e o = (o (evalArithExpr ae1 t) (evalArithExpr ae2 t))

evalStatement :: Statement -> SymbolTable -> SymbolTable
evalStatement s t = nubBy ((==) `on` fst) $ evalStatement' s t

evalStatement' :: Statement -> SymbolTable -> SymbolTable
evalStatement' (Assignment var expr) t = (var, (evalArithExpr expr t)) : t
evalStatement' (Skip) t = t
evalStatement' (Sequential s1 s2) t = evalStatement s2 (evalStatement s1 t)
evalStatement' (IfThenElse be s1 s2) t = if (evalBoolExpr be t) then (evalStatement s1 t) else (evalStatement s2 t)
evalStatement' w@(While be s) t = if (evalBoolExpr be t) then (evalStatement w (evalStatement s t)) else t

emptyTable = ([] :: [(String,Integer)])

