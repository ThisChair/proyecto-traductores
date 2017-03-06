module Express where

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
express (ESum (Tree.Sum l r))  = addi (ESum (Tree.Sum l r))
express (EDif (Dif l r))       = subs (EDif (Dif l r))
express (EMul (Mul l r))       = multi (EMul (Mul l r))
express (EDiv (Div l r))       = divi (EDiv (Div l r))
express (EMod (Mod l r))       = rest (EMod (Mod l r))
express (EDivI (DivI l r))     = diviI (EDivI (DivI l r))
express (EModI (ModI l r))     = restI (EModI (ModI l r))
express (EOr (Or l r))         = disy (EOr (Or l r))
express (EAnd (And l r))       = conj (EAnd (And l r))
express (EGeq (Geq l r))       = greatEq (EGeq (Geq l r))
express (EGr (Gr l r))         = greater (EGr (Gr l r))
express (ELeq (Leq l r))       = lessEq (ELeq (Leq l r))
express (ELess (Less l r))     = lesser (ELess (Less l r))
express (ENeq (Neq l r))       = notEq (ENeq (Neq l r))
express (EEqual (Equal l r))   = equal (EEqual (Equal l r))
express (ENeg  e)              = negat (ENeg  e)
express (ENot  e)              = nega (ENot  e)
express (EToken (TIdent p id)) = identifier (EToken (TIdent p id))
express (EToken (TTrue p))     = constTrue (EToken (TTrue p))
express (EToken (TFalse p))    = constFalse (EToken (TFalse p))
express (EToken (TNum p n))    = numb (EToken (TNum p n))
express (EFCall (FCall t exp)) = fCall (EFCall (FCall t exp))


--Sumas. Devuelve tipo Number y la suma de los valores.
addi (ESum (Tree.Sum l r)) = do
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
subs (EDif (Dif l r)) = do
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
multi (EMul (Mul l r)) = do
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
divi (EDiv (Div l r)) = do
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
rest (EMod (Mod l r)) = do
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
diviI (EDivI (DivI l r)) = do
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
restI (EModI (ModI l r)) = do
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
disy (EOr (Or l r)) = do
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
conj (EAnd (And l r)) = do
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
greatEq (EGeq (Geq l r)) = do
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
greater (EGr (Gr l r)) = do
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
lessEq (ELeq (Leq l r)) = do
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
lesser (ELess (Less l r)) = do
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
notEq (ENeq (Neq l r)) = do
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
equal (EEqual (Equal l r)) = do
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
negat (ENeg  e) = do
  var <- express e
  case t var of
    Number -> do return()
    Boolean -> do return() -- Error ):
  let val = - num var
  return (Variable Number val False)


--Negación. Devuelve tipo Boolean y el valor.
nega (ENot  e) = do
  var <- express e
  case t var of
    Number -> do return() -- Error ):
    Boolean -> do return()
  let val = not $ bool var
  return (Variable Boolean 0 val)


--Identificadores. Devuelve el tipo y el valor.
identifier (EToken (TIdent _ id)) = do
  scope <- get
  let tableSym  = sym scope
  let var = findSym tableSym id
  case var of
    Nothing -> do return() -- Error ):
    Just _ -> do return () 
  let x = fromJust var 
  return x

  

--Constante True. Devuelve tipo Boolean y True.
constTrue (EToken (TTrue _)) = do
  return (Variable Boolean 0 True)


--Constante False. Devuelve tipo Boolean y False.
constFalse (EToken (TFalse _)) = do
  return (Variable Boolean 0 False)


--Número. Devuelve tipo Number y el valor.
numb (EToken (TNum _ n)) = do
  return (Variable Number n False)


-- Llamada a funcion
fCall (EFCall (FCall t exp)) = do funcCall (FCall t exp)


-- Funcion recursiva que verifica si los parametros coinciden en una
-- llamada a una funcion
checkPars :: [Type] -> [Exp] -> RetMonad ()
checkPars [] []         = do return ()                                  -- Termina la verificacion con exito
checkPars [] (_:_)      = do return ()                                  -- ERROR, LLAMADA A FUNCION CON CANTIDAD DE PARAMETROS INCORRECTOS
checkPars (_:_) []      = do return ()                                  -- ERROR, LLAMADA A FUNCION CON CANTIDAD DE PARAMETROS INCORRECTOS    
checkPars (ty:ts) (e:es) = do
  val <- express e                                                      -- calcular valor de la siguiente expresion
  case (ty /= (t val))  of                                              -- Verificar que coincida el tipo del siguiente parametro
    True  -> do return ()                                               -- ERROR NO COINCIDE TIPO DE PARAMETRO
    False -> checkPars ts es                                            -- continuar verificando

-- Instruccion, llamada a una funcion
funcCall :: FCall -> RetMonad Variable
funcCall (FCall (TIdent _ id) exps) = do
  scope <- get
  let funcId = findFunc (func scope) id
  case funcId of
    Nothing   -> do return nullVariable                             -- ERROR, FUNCION NO DECLARADA
    Just val  -> do                                                 -- La funcion si esta declarada
      checkPars (parameters val) (exps)                             -- Verificar que los parametros coincidan en numero y tipo
      return (Variable (ret val) 0 False)                           -- Retorna el valor de la funcion
