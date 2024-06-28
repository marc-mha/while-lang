module Interpreter (execute, Mapping, State, Config (..)) where

import Data.Maybe (fromMaybe)
import Parser (AExp (..), BExp (..), Stmt (..), Variable (..))

type Mapping = (Variable, Integer)

type State = [Mapping]

data Config = Config [Stmt] State

instance Show Config where
  show (Config program state) = "<" ++ show program ++ ", " ++ show state ++ ">"

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

step :: Config -> Either State Config
step (Config program state) = case program of
  [] -> Left state
  (instruction : next) -> Right $ case instruction of
    IfThenElse b s t -> Config ((if evalB b state then s else t) ++ next) state
    w@(WhileDo b s) -> Config (IfThenElse b (s ++ [w]) [Skip] : next) state
    Assign x expr -> Config next (assign state x expr)
    Skip -> Config next state

execute :: Config -> State
execute config = case step config of
  Left state -> state
  Right config' -> execute config'
