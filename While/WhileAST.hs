module WhileAST where
-- AST of the very simple While language, as described in
-- F. Nielson, H R. Nielson, C. Hankin: Principles of Program Analysis

data ArithOp = Plus | Minus | Mul | Div
  deriving (Show,Eq)

type Variable = String
data ArithExpr = Variable String | Constant Integer | ArithBinOpExpr ArithExpr ArithOp ArithExpr
  deriving (Show, Eq)

data BoolOp = BoolEq
  deriving (Show,Eq)

data ArithRelOp = Lt | Gt | Lte | Gte | ArithEq
  deriving (Show,Eq)

data BoolExpr = True | False | Not BoolExpr | BoolBinOpExpr BoolExpr BoolOp BoolExpr | ArithBoolOpExpr ArithExpr ArithRelOp ArithExpr
  deriving (Show)

data Statement = Assignment String ArithExpr | Skip | Sequential Statement Statement | IfThenElse BoolExpr Statement Statement | While BoolExpr Statement
  deriving (Show)

type Program = Statement
