module Main (main) where

import Interpreter (Config (..), Mapping, State, execute)
import Parser (Stmt, Variable (..), parseProgram)
import System.Environment (getArgs)

getFilename :: IO String
getFilename = do
  args <- getArgs
  return
    ( case args of
        [] -> error "File name expected."
        (fn : _) -> fn
    )

generateAST :: String -> [Stmt]
generateAST source = case parseProgram source of
  Nothing -> error "Invalid syntax."
  Just program -> program

showMapping :: Mapping -> String
showMapping (Variable x, n) = x ++ " |-> " ++ show n

showState :: State -> String
showState state = "[ " ++ pstate state ++ " ]"
  where
    pstate [] = ""
    pstate [m] = showMapping m
    pstate (m : ms) = showMapping m ++ ", " ++ pstate ms

main :: IO ()
main = do
  filename <- getFilename
  source <- readFile filename
  let ast = generateAST source
  let final = execute (ast, [])
  putStrLn (showState final)
