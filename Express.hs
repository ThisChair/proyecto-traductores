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
express (ESum (Tree.Sum l r))  = addi (Tree.Sum l r)
express (EDif (Dif l r))       = subs (Dif l r)
express (EMul (Mul l r))       = multi (Mul l r)
express (EDiv (Div l r))       = divi (Div l r)
express (EMod (Mod l r))       = rest (Mod l r)
express (EDivI (DivI l r))     = diviI (DivI l r)
express (EModI (ModI l r))     = restI (ModI l r)
express (EOr (Or l r))         = disy (Or l r)
express (EAnd (And l r))       = conj (And l r)
express (EGeq (Geq l r))       = greatEq (Geq l r)
express (EGr (Gr l r))         = greater (Gr l r)
express (ELeq (Leq l r))       = lessEq (Leq l r)
express (ELess (Less l r))     = lesser (Less l r)
express (ENeq (Neq l r))       = notEq (Neq l r)
express (EEqual (Equal l r))   = equal (Equal l r)
express (ENeg e)               = negat e
express (ENot e)               = nega e
express (EToken (TIdent p id)) = identifier (TIdent p id)
express (EToken (TTrue p))     = constTrue (TTrue p)
express (EToken (TFalse p))    = constFalse (TFalse p)
express (EToken (TNum p n))    = numb (TNum p n)
express (EFCall (FCall t exp)) = fCall (FCall t exp)


--Sumas. Devuelve tipo Number y la suma de los valores.
addi :: Tree.Sum -> RetMonad Variable
addi (Tree.Sum l r) = do
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
subs :: Dif -> RetMonad Variable
subs (Dif l r) = do
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
multi :: Mul -> RetMonad Variable 
multi (Mul l r) = do
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
divi :: Div -> RetMonad Variable
divi (Div l r) = do
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
rest :: Mod -> RetMonad Variable
rest (Mod l r) = do
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
diviI :: DivI -> RetMonad Variable
diviI (DivI l r) = do
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
restI :: ModI -> RetMonad Variable
restI (ModI l r) = do
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
disy :: Or -> RetMonad Variable
disy (Or l r) = do
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
conj :: And -> RetMonad Variable
conj (And l r) = do
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
greatEq :: Geq -> RetMonad Variable
greatEq (Geq l r) = do
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
greater :: Gr -> RetMonad Variable 
greater (Gr l r) = do
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
lessEq :: Leq -> RetMonad Variable
lessEq (Leq l r) = do
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
lesser :: Less -> RetMonad Variable
lesser (Less l r) = do
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
notEq :: Neq -> RetMonad Variable
notEq (Neq l r) = do
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
equal :: Equal -> RetMonad Variable
equal (Equal l r) = do
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
negat :: Exp -> RetMonad Variable
negat e = do
  var <- express e
  case t var of
    Number -> do return()
    Boolean -> do return() -- Error ):
  let val = - num var
  return (Variable Number val False)


--Negación. Devuelve tipo Boolean y el valor.
nega :: Exp -> RetMonad Variable
nega (ENot  e) = do
  var <- express e
  case t var of
    Number -> do return() -- Error ):
    Boolean -> do return()
  let val = not $ bool var
  return (Variable Boolean 0 val)


--Identificadores. Devuelve el tipo y el valor.
identifier :: Token -> RetMonad Variable
identifier (TIdent _ id) = do
  scope <- get
  let tableSym  = sym scope
  let var = findSym tableSym id
  case var of
    Nothing -> do return() -- Error ):
    Just _ -> do return () 
  let x = fromJust var 
  return x


--Constante True. Devuelve tipo Boolean y True.
constTrue :: Token -> RetMonad Variable
constTrue (TTrue _) = do
  return (Variable Boolean 0 True)


--Constante False. Devuelve tipo Boolean y False.
constFalse :: Token -> RetMonad Variable
constFalse (TFalse _) = do
  return (Variable Boolean 0 False)


--Número. Devuelve tipo Number y el valor.
numb :: Token -> RetMonad Variable
numb (TNum _ n) = do
  return (Variable Number n False)


-- Llamada a funcion
fCall :: FCall -> RetMonad Variable
fCall (FCall t exp) = do funcCall (FCall t exp)


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
