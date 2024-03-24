module Main (main) where

import System.Environment (getArgs)
import Parser (parseProgram, Variable(..), Value(..), Stmt)
import Interpreter (execute, Mapping, State, Config(..))

getFilename :: IO String
getFilename = do
  args <- getArgs
  return (
    case args of
      [] -> error "File name expected."
      (fn : _) -> fn
    )

generateAST :: String -> [Stmt]
generateAST source = case parseProgram source of
  Nothing -> error "Invalid syntax."
  Just program -> program

showMapping :: Mapping -> String
showMapping (Variable x, Value n) = x ++ " |-> " ++ show n

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
  let final = execute (Config ast [])
  putStrLn (showState final)
