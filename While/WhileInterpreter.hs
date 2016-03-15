import WhileAST
import WhileEvaluate
import WhileParser

import Data.List
import Data.Function
import System.IO
import System.Environment

import Text.Parsec (parse)

runProgram :: Program -> SymbolTable -> SymbolTable
runProgram p t = nubBy ((==) `on` fst) (evalStatement p t)

main :: IO ()
main = do
  putStrLn "While interpreter"
  (fn:_) <- getArgs
  withFile fn ReadMode (\f -> do
    contents <- hGetContents f
    let p = parse WhileParser.programParser fn contents
    let program = case p of Left err -> error (show err)
                            Right s -> s
    putStrLn "Parsed program: "
    putStrLn (show program)
    putStrLn "Executing program..."
    let table = runProgram program emptyTable
    putStrLn "Finished. Symbol table:"
    putStrLn (show table)
    )
  putStrLn "End."
