module Interpreter (execute, Mapping, State, Config (..)) where

import Data.Maybe (fromMaybe)
import Parser (AExp (..), BExp (..), Stmt (..), Value (..), Variable (..))

instance Num Value where
  (Value a) + (Value b) = Value (a + b)
  (Value a) - (Value b) = Value (a - b)
  (Value a) * (Value b) = Value (a * b)
  abs (Value a) = Value (abs a)
  signum (Value a) = Value (signum a)
  fromInteger = Value

instance Eq Value where
  (Value a) == (Value b) = a == b

instance Ord Value where
  (Value a) `compare` (Value b) = a `compare` b

type Mapping = (Variable, Value)

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

evalA :: AExp -> State -> Value
evalA (Num n) _ = n
evalA (Var x) state = fromMaybe (Value 0) (lookup x state)
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
  (instruction : next) -> case instruction of
    x `Assign` expr -> Right $ Config next $ assign state x expr
    Skip -> Right $ Config next state
    IfThenElse b s t ->
      let u = if evalB b state then s else t
       in Right $ Config (u ++ next) state
    WhileDo b s ->
      let u = IfThenElse b (s ++ [WhileDo b s]) [Skip]
       in Right $ Config (u : next) state

execute :: Config -> State
execute config = case step config of
  Left state -> state
  Right config' -> execute config'
