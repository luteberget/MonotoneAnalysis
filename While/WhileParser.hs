module WhileParser where

import WhileAST
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Token
import Text.Parsec.Language (javaStyle)
import Control.Applicative ((<*))

lexer = makeTokenParser javaStyle
arithExprParser = buildExpressionParser arithTable arithExprTermParser
arithExprTermParser = parens lexer arithExprParser
   <|> (do { n <- natural lexer; return (Constant n) } )
   <|> (do { var <- identifier lexer; return (Variable var)})

arithTable = [
   [ Infix ( do { reservedOp lexer "*" ; return (\x y -> (ArithBinOpExpr x Mul y ) ) }) AssocLeft,
     Infix ( do { reservedOp lexer "/" ; return (\x y -> (ArithBinOpExpr x Div y ) ) }) AssocLeft
   ],
   [ Infix ( do { reservedOp lexer "+" ; return (\x y -> (ArithBinOpExpr x Plus y ) ) }) AssocLeft,
     Infix ( do { reservedOp lexer "-" ; return (\x y -> (ArithBinOpExpr x Minus y ) ) }) AssocLeft
   ]
 ]

boolExprParser = buildExpressionParser boolTable boolExprTermParser
boolExprTermParser = try (parens lexer boolExprParser)
  <|> try ( do { reservedOp lexer "true"; return WhileAST.True})
  <|> try ( do { reservedOp lexer "false"; return WhileAST.False})
  <|> ( do {
          ae1 <- arithExprParser <* spaces;
          op <- arithRelOpParser <* spaces;
          ae2 <- arithExprParser <* spaces;
          return (ArithBoolOpExpr ae1 op ae2)
           })

arithRelOpParser =
      try ( do { reservedOp lexer "<=" ; return Lte })
  <|> try ( do { reservedOp lexer ">=" ; return Gte })
  <|>     ( do { reservedOp lexer "<" ; return Lt })
  <|>     ( do { reservedOp lexer ">" ; return Gt })

boolTable = [
  [ Prefix ( do  { reservedOp lexer "!"; return (\x -> (Not x))})],
  [ Infix ( do { reservedOp lexer "=="; return (\x y -> (BoolBinOpExpr x BoolEq y))}) AssocLeft]
 ]


statementSeparator :: Parsec String () ()
statementSeparator = spaces >> char ';' >> spaces

skipParser = do { string "skip"; return Skip }
assignmentParser = do
  var <- identifier lexer <* spaces
  string ":=" <* spaces
  expr <- arithExprParser <* spaces
  return (Assignment var expr)

ifThenElseParser = do
  string "if" <* spaces
  condition <- boolExprParser <* spaces
  string "then" <* spaces
  ifBlock <- statementParser <* spaces
  string "else" <* spaces
  elseBlock <- statementParser <* spaces
  string "endif" <* spaces
  return (IfThenElse condition ifBlock elseBlock)

whileParser = do
  string "while" <* spaces
  condition <- boolExprParser <* spaces
  string "do" <* spaces
  block <- statementParser <* spaces
  string "endwhile" <* spaces
  return (While condition block)

statementParser :: Parsec String () Statement
statementParser = do
  spaces
  s1 <- try skipParser <|> try assignmentParser <|> try ifThenElseParser <|> try whileParser <* spaces
  s2 <- option s1 (try (do
                             statementSeparator
                             s <- statementParser
                             return (Sequential s1 s)
                             ))
  return s2

programParser = do
  s <- statementParser <* spaces
  eof
  return s
