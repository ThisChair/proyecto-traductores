module Main(main) where

import Control.Monad.RWS
import Tree
import TokenInfo
import Lexer
import RetMonad
import Data.Sequence as S
import Data.Map as M
import Data.Set as Set
import Prelude as P
import Data.Maybe

--Maneja las expresiones
express :: Exp -> RetMonad Variable
--Sumas. Devuelve tipo Number y la suma de los valores.
express (ESum input) = do
  exp1 <- express(leftEx)
  exp2 <- express(rightEx)
  case t exp1 of
    Number -> do return()
    Boolean -> do return() -- Error ):
  case t exp2 of
    Number -> do return()
    Boolean -> do return() -- Error ):
  let val = num exp1 + num exp2
  return (Variable Number val False)
  where (leftEx, rightEx) = getExp input
        getExp (Tree.Sum l r) = (l, r)
--Restas. Devuelve tipo Number y la resta de los valores.
express (EDif input) = do
  exp1 <- express(leftEx)
  exp2 <- express(rightEx)
  case t exp1 of
    Number -> do return()
    Boolean -> do return() -- Error ):
  case t exp2 of
    Number -> do return()
    Boolean -> do return() -- Error ):
  let val = num exp1 - num exp2
  return (Variable Number val False)
  where (leftEx, rightEx) = getExp input
        getExp (Dif l r) = (l, r)
--Multiplicaciones. Devuelve tipo Number y el producto de los valores.
express (EMul input) = do
  exp1 <- express(leftEx)
  exp2 <- express(rightEx)
  case t exp1 of
    Number -> do return()
    Boolean -> do return() -- Error ):
  case t exp2 of
    Number -> do return()
    Boolean -> do return() -- Error ):
  let val = num exp1 * num exp2
  return (Variable Number val False)
  where (leftEx, rightEx) = getExp input
        getExp (Mul l r) = (l, r)
--Divisiones. Devuelve tipo Number y el cociente de los valores.
express (EDiv input) = do
  exp1 <- express(leftEx)
  exp2 <- express(rightEx)
  case t exp1 of
    Number -> do return()
    Boolean -> do return() -- Error ):
  case t exp2 of
    Number -> do return()
    Boolean -> do return() -- Error ):
  case num exp2 of
    0 -> do return() -- Error ):
    _ -> do return()
  let val = num exp1 / num exp2
  return (Variable Number val False)
  where (leftEx, rightEx) = getExp input
        getExp (Div l r) = (l, r)
--Resto. Devuelve tipo Number y el resto de la división de los valores.
express (EMod input) = do
  exp1 <- express(leftEx)
  exp2 <- express(rightEx)
  case t exp1 of
    Number -> do return()
    Boolean -> do return() -- Error ):
  case t exp2 of
    Number -> do return()
    Boolean -> do return() -- Error ):
  case num exp2 of
    0 -> do return() -- Error ):
    _ -> do return()
  let int1 = round (num exp1)
  let int2 = round (num exp2)
  case (num exp1 /= int1) of
    True -> return() -- Error ):
    False -> return()
  case (num exp2 /= int2) of
    True -> return() -- Error ):
    False -> return()
  let val = int1 `mod` int2
  return (Variable Number val False)
  where (leftEx, rightEx) = getExp input
        getExp (Mod l r) = (l, r)
--Identificadores. Devuelve el tipo y el valor.
express (EToken (TIdent _ id)) = do
  scope <- get
  let tableSym  = sym scope
  let var = findSym tableSym id
  case var of
    Nothing -> do return() -- Error ):
    Just _ -> do return () 
  let x = fromJust var 
  return x
--Constante True. Devuelve tipo Boolean y True.
express (EToken (TTrue _)) = do
  return (Variable Boolean 0 True)
--Constante False. Devuelve tipo Boolean y False.
express (EToken (TFalse _)) = do
  return (Variable Boolean 0 False)
--Número. Devuelve tipo Number y el valor.
express (EToken (TNum _ n)) = do
  return (Variable Boolean n False)


main :: IO ()
main = putStrLn "Ok"