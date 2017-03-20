module Iterator where
import Control.Monad.RWS
import RunMonad
import Tree
import RunExpress
import Data.Map as M
import Prelude as P

runWhile :: Exp -> [Ins] -> RunMonad ()
runWhile cond is = do
  valExp <- runExpress cond
  case (bool valExp) of
    True -> do P.mapM_ instruction is
                runWhile cond is
    False -> do return()
    
runRepeat :: Integral a => a -> [Ins] -> RunMonad ()
runRepeat n is = do
  case n of
    0 -> do return()
    _ -> do P.mapM_ instrucion is
            runRepeat n-1 is

runFor :: String -> Variable -> [Ins] -> RunMonad()
runFor id quota is = do
  scope <- get
  let tableSym  = sym scope
  let var = fromJust (findSym tableSym id)
  let counter = num var
  case (counter > num quota) of
    True -> do return()
    False -> do P.mapM_ instruction is
                modify(modifyTable $ modifySym id (Variable Number (counter + 1) False))
                runFor id quota is

runForBy :: String -> Variable -> Variable -> [Ins] -> RunMonad()
runForBy id quota inc is = do
  scope <- get
  let tableSym  = sym scope
  let var = fromJust (findSym tableSym id)
  let counter = num var
  case (counter > num quota) of
    True -> do return()
    False -> do P.mapM_ instruction is
                modify(modifyTable $ modifySym id (Variable Number (counter + (num increase)) False))
                runFor id quota inc is

