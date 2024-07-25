module Interpreter (execute, Mapping, State) where

import Data.Maybe (fromMaybe)
import Parser (AExp (..), BExp (..), Stmt (..), Variable (..))

type Mapping = (Variable, Integer)

type State = [Mapping]

type Config = ([Stmt], State)

modifyState :: State -> Mapping -> State
modifyState (m : ms) new_m =
  if fst m == fst new_m
    then
      -- If this is the correct variable, then replace the variable and
      -- return the rest of the state as there cannot be duplicates.
      new_m : ms
    else
      -- Otherwise, leave the current map and comb through the rest.
      m : modifyState ms new_m
-- If the end is reached, then variable must not have been found so
-- just append to the end.
modifyState [] m = [m]

evalA :: AExp -> State -> Integer
evalA (Num n) _ = n
evalA (Var x) state = fromMaybe 0 (lookup x state)
evalA (a `Add` b) state = evalA a state + evalA b state
evalA (a `Sub` b) state = evalA a state - evalA b state
evalA (a `Mul` b) state = evalA a state * evalA b state

evalB :: BExp -> State -> Bool
evalB (BoolConst True) _ = True
evalB (BoolConst False) _ = False
evalB (a `Equals` b) state = evalA a state == evalA b state
evalB (a `LessThanEquals` b) state = evalA a state <= evalA b state
evalB (Not a) state = not $ evalB a state
evalB (a `And` b) state = evalB a state && evalB b state

assign :: State -> Variable -> AExp -> State
assign state x expr = modifyState state (x, evalA expr state)

step :: Config -> Config
step ([], state) = ([], state)
step (instruction : next, state) = case instruction of
  IfThenElse b s t -> ((if evalB b state then s else t) ++ next, state)
  w@(WhileDo b s) -> (IfThenElse b (s ++ [w]) [Skip] : next, state)
  Assign x expr -> (next, assign state x expr)
  Skip -> (next, state)

execute :: Config -> State
execute ([], state) = state
execute config = execute $ step config
