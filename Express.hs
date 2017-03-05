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
express (ESum (Tree.Sum l r)) = do
  exp1 <- express l
  exp2 <- express r
  case t exp1 of
    Number -> do return()
    Boolean -> do return() -- Error ):
  case t exp2 of
    Number -> do return()
    Boolean -> do return() -- Error ):
  let val = num exp1 + num exp2
  return (Variable Number val False)

  
--Restas. Devuelve tipo Number y la resta de los valores.
express (EDif (Dif l r)) = do
  exp1 <- express l
  exp2 <- express r
  case t exp1 of
    Number -> do return()
    Boolean -> do return() -- Error ):
  case t exp2 of
    Number -> do return()
    Boolean -> do return() -- Error ):
  let val = num exp1 - num exp2
  return (Variable Number val False)


--Multiplicaciones. Devuelve tipo Number y el producto de los valores.
express (EMul (Mul l r)) = do
  exp1 <- express l
  exp2 <- express r
  case t exp1 of
    Number -> do return()
    Boolean -> do return() -- Error ):
  case t exp2 of
    Number -> do return()
    Boolean -> do return() -- Error ):
  let val = num exp1 * num exp2
  return (Variable Number val False)


--Divisiones. Devuelve tipo Number y el cociente de los valores.
express (EDiv (Div l r)) = do
  exp1 <- express l
  exp2 <- express r
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


--Resto. Devuelve tipo Number y el resto de la división de los valores.
express (EMod (Mod l r)) = do
  exp1 <- express l
  exp2 <- express r
  case t exp1 of
    Number -> do return()
    Boolean -> do return() -- Error ):
  case t exp2 of
    Number -> do return()
    Boolean -> do return() -- Error ):
  case num exp2 of
    0 -> do return() -- Error ):
    _ -> do return()
  let dob1 = num exp1
  let dob2 = num exp2
  let int1 = round dob1
  let int2 = round dob2
  case (dob1 /= fromInteger int1) of
    True -> return() -- Error ):
    False -> return()
  case (dob2 /= fromInteger int2) of
    True -> return() -- Error ):
    False -> return()
  let val = fromInteger (int1 `mod` int2)
  return (Variable Number val False)


--División entera. Devuelve tipo Number y la división entera de los valores.
express (EDivI (DivI l r)) = do
  exp1 <- express l
  exp2 <- express r
  case t exp1 of
    Number -> do return()
    Boolean -> do return() -- Error ):
  case t exp2 of
    Number -> do return()
    Boolean -> do return() -- Error ):
  case num exp2 of
    0 -> do return() -- Error ):
    _ -> do return()
  let dob1 = num exp1
  let dob2 = num exp2
  let int1 = round dob1
  let int2 = round dob2
  case (dob1 /= fromInteger int1) of
    True -> return() -- Error ):
    False -> return()
  case (dob2 /= fromInteger int2) of
    True -> return() -- Error ):
    False -> return()
  let val = fromInteger (int1 `div` int2)
  return (Variable Number val False)


--Resto entero. Devuelve tipo Number y el resto de la división de los valores.
express (EModI (ModI l r)) = do
  exp1 <- express l
  exp2 <- express r
  case t exp1 of
    Number -> do return()
    Boolean -> do return() -- Error ):
  case t exp2 of
    Number -> do return()
    Boolean -> do return() -- Error ):
  case num exp2 of
    0 -> do return() -- Error ):
    _ -> do return()
  let dob1 = num exp1
  let dob2 = num exp2
  let int1 = round dob1
  let int2 = round dob2
  case (dob1 /= fromInteger int1) of
    True -> return() -- Error ):
    False -> return()
  case (dob2 /= fromInteger int2) of
    True -> return() -- Error ):
    False -> return()
  let val = fromInteger (int1 `mod` int2)
  return (Variable Number val False)


--Disyunción. Devuelve tipo Boolean y la disyunción de los valores.
express (EOr (Or l r)) = do
  exp1 <- express l
  exp2 <- express r
  case t exp1 of
    Number -> do return() -- Error ):
    Boolean -> do return()
  case t exp2 of
    Number -> do return() -- Error ):
    Boolean -> do return()
  let val = bool exp1 || bool exp2
  return (Variable Boolean 0 val)


--Conjunción. Devuelve tipo Boolean y la conjunción de los valores.
express (EAnd (And l r)) = do
  exp1 <- express l
  exp2 <- express r
  case t exp1 of
    Number -> do return() -- Error ):
    Boolean -> do return()
  case t exp2 of
    Number -> do return() -- Error ):
    Boolean -> do return()
  let val = bool exp1 && bool exp2
  return (Variable Boolean 0 val)


--Mayor o igual que. Devuelve tipo Boolean y True si cumple la relación.
express (EGeq (Geq l r)) = do
  exp1 <- express l
  exp2 <- express r
  case t exp1 of
    Number -> do return()
    Boolean -> do return() -- Error ):
  case t exp2 of
    Number -> do return()
    Boolean -> do return() -- Error ):
  let val = num exp1 >= num exp2
  return (Variable Boolean 0 val)


--Mayor que. Devuelve tipo Boolean y True si cumple la relación.
express (EGr (Gr l r)) = do
  exp1 <- express l
  exp2 <- express r
  case t exp1 of
    Number -> do return()
    Boolean -> do return() -- Error ):
  case t exp2 of
    Number -> do return()
    Boolean -> do return() -- Error ):
  let val = num exp1 > num exp2
  return (Variable Boolean 0 val)


--Menor o igual que. Devuelve tipo Boolean y True si cumple la relación.
express (ELeq (Leq l r)) = do
  exp1 <- express l
  exp2 <- express r
  case t exp1 of
    Number -> do return()
    Boolean -> do return() -- Error ):
  case t exp2 of
    Number -> do return()
    Boolean -> do return() -- Error ):
  let val = num exp1 <= num exp2
  return (Variable Boolean 0 val)


--Menor que. Devuelve tipo Boolean y True si cumple la relación.
express (ELess (Less l r)) = do
  exp1 <- express l
  exp2 <- express r
  case t exp1 of
    Number -> do return()
    Boolean -> do return() -- Error ):
  case t exp2 of
    Number -> do return()
    Boolean -> do return() -- Error ):
  let val = num exp1 < num exp2
  return (Variable Boolean 0 val)


--Distinto de. Devuelve tipo Boolean y True si cumple la relación.
express (ENeq (Neq l r)) = do
  exp1 <- express l
  exp2 <- express r
  let eqType = t exp1 == t exp2
  case eqType of
    True -> do return()
    False -> do return() -- Error ):
  let val = case t exp1 of 
              Number -> num exp1 /= num exp2
              Boolean -> bool exp1 /= bool exp2
  return (Variable Boolean 0 val)


--Distinto de. Devuelve tipo Boolean y True si cumple la relación.
express (EEqual (Equal l r)) = do
  exp1 <- express l
  exp2 <- express r
  let eqType = t exp1 == t exp2
  case eqType of
    True -> do return()
    False -> do return() -- Error ):
  let val = case t exp1 of 
              Number -> num exp1 == num exp2
              Boolean -> bool exp1 == bool exp2
  return (Variable Boolean 0 val)


--Negativo. Devuelve tipo Number y el valor.
express (ENeg  e) = do
  var <- express e
  case t var of
    Number -> do return()
    Boolean -> do return() -- Error ):
  let val = - num var
  return (Variable Number val False)


--Negación. Devuelve tipo Boolean y el valor.
express (ENot  e) = do
  var <- express e
  case t var of
    Number -> do return() -- Error ):
    Boolean -> do return()
  let val = not $ bool var
  return (Variable Boolean 0 val)


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