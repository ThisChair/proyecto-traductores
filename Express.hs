module Express where

import Control.Monad.State.Strict
import Tree
import TokenInfo
import Lexer
import RetMonad
import Data.Sequence as S
import Data.Map as M
import Data.Set as Set
import Prelude as P
import Data.Maybe
import ContextError

--Maneja las expresiones
express :: Exp -> RetMonad Type
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
express (EFCall (FCall t exp)) = funcCall (FCall t exp)


--Sumas. Devuelve tipo Number y la suma de los valores.
addi :: Tree.Sum -> RetMonad Type
addi (Tree.Sum l r) = do
  exp1 <- express l
  exp2 <- express r
  case exp1 of
    Number -> do return()
    Boolean -> do errUnexpectedType l Boolean Number
    Void -> do errVoidExp l
  case exp2 of
    Number -> do return()
    Boolean -> do errUnexpectedType r Boolean Number
    Void -> do errVoidExp r
  return Number

  
--Restas. Devuelve tipo Number y la resta de los valores.
subs :: Dif -> RetMonad Type
subs (Dif l r) = do
  exp1 <- express l
  exp2 <- express r
  case exp1 of
    Number -> do return()
    Boolean -> do errUnexpectedType l Boolean Number
    Void -> do errVoidExp l
  case exp2 of
    Number -> do return()
    Boolean -> do errUnexpectedType r Boolean Number
    Void -> do errVoidExp r
  return Number


--Multiplicaciones. Devuelve tipo Number y el producto de los valores.
multi :: Mul -> RetMonad Type
multi (Mul l r) = do
  exp1 <- express l
  exp2 <- express r
  case exp1 of
    Number -> do return()
    Boolean -> do errUnexpectedType l Boolean Number
    Void -> do errVoidExp l
  case exp2 of
    Number -> do return()
    Boolean -> do errUnexpectedType r Boolean Number
    Void -> do errVoidExp r
  return Number


--Divisiones. Devuelve tipo Number y el cociente de los valores.
divi :: Div -> RetMonad Type
divi (Div l r) = do
  exp1 <- express l
  exp2 <- express r
  case exp1 of
    Number -> do return()
    Boolean -> do errUnexpectedType l Boolean Number
    Void -> do errVoidExp l
  case exp2 of
    Number -> do return()
    Boolean -> do errUnexpectedType r Boolean Number
    Void -> do errVoidExp r
  return Number


--Resto. Devuelve tipo Number y el resto de la división de los valores.
rest :: Mod -> RetMonad Type
rest (Mod l r) = do
  exp1 <- express l
  exp2 <- express r
  case exp1 of
    Number -> do return()
    Boolean -> do errUnexpectedType l Boolean Number
    Void -> do errVoidExp l
  case exp2 of
    Number -> do return()
    Boolean -> do errUnexpectedType r Boolean Number
    Void -> do errVoidExp r
  return Number


--División entera. Devuelve tipo Number y la división entera de los valores.
diviI :: DivI -> RetMonad Type
diviI (DivI l r) = do
  exp1 <- express l
  exp2 <- express r
  case exp1 of
    Number -> do return()
    Boolean -> do errUnexpectedType l Boolean Number
    Void -> do errVoidExp l
  case exp2 of
    Number -> do return()
    Boolean -> do errUnexpectedType r Boolean Number
    Void -> do errVoidExp r
  return Number


--Resto entero. Devuelve tipo Number y el resto de la división de los valores.
restI :: ModI -> RetMonad Type
restI (ModI l r) = do
  exp1 <- express l
  exp2 <- express r
  case exp1 of
    Number -> do return()
    Boolean -> do errUnexpectedType l Boolean Number
    Void -> do errVoidExp l
  case exp2 of
    Number -> do return()
    Boolean -> do errUnexpectedType r Boolean Number
    Void -> do errVoidExp r
  return Number


--Disyunción. Devuelve tipo Boolean y la disyunción de los valores.
disy :: Or -> RetMonad Type
disy (Or l r) = do
  exp1 <- express l
  exp2 <- express r
  case exp1 of
    Number -> do errUnexpectedType l Number Boolean
    Boolean -> do return()
    Void -> do errVoidExp l
  case exp2 of
    Number -> do errUnexpectedType r Number Boolean
    Boolean -> do return()
    Void -> do errVoidExp r
  return Boolean


--Conjunción. Devuelve tipo Boolean y la conjunción de los valores.
conj :: And -> RetMonad Type
conj (And l r) = do
  exp1 <- express l
  exp2 <- express r
  case exp1 of
    Number -> do errUnexpectedType l Number Boolean
    Boolean -> do return()
    Void -> do errVoidExp l
  case exp2 of
    Number -> do errUnexpectedType r Number Boolean
    Boolean -> do return()
    Void -> do errVoidExp r
  return Boolean


--Mayor o igual que. Devuelve tipo Boolean y True si cumple la relación.
greatEq :: Geq -> RetMonad Type
greatEq (Geq l r) = do
  exp1 <- express l
  exp2 <- express r
  case exp1 of
    Number -> do return()
    Boolean -> do errUnexpectedType l Boolean Number
    Void -> do errVoidExp l
  case exp2 of
    Number -> do return()
    Boolean -> do errUnexpectedType r Boolean Number
    Void -> do errVoidExp r
  return Boolean


--Mayor que. Devuelve tipo Boolean y True si cumple la relación.
greater :: Gr -> RetMonad Type
greater (Gr l r) = do
  exp1 <- express l
  exp2 <- express r
  case exp1 of
    Number -> do return()
    Boolean -> do errUnexpectedType l Boolean Number
    Void -> do errVoidExp l
  case exp2 of
    Number -> do return()
    Boolean -> do errUnexpectedType r Boolean Number
    Void -> do errVoidExp r
  return Boolean


--Menor o igual que. Devuelve tipo Boolean y True si cumple la relación.
lessEq :: Leq -> RetMonad Type
lessEq (Leq l r) = do
  exp1 <- express l
  exp2 <- express r
  case exp1 of
    Number -> do return()
    Boolean -> do errUnexpectedType l Boolean Number
    Void -> do errVoidExp l
  case exp2 of
    Number -> do return()
    Boolean -> do errUnexpectedType r Boolean Number
    Void -> do errVoidExp r
  return Boolean


--Menor que. Devuelve tipo Boolean y True si cumple la relación.
lesser :: Less -> RetMonad Type
lesser (Less l r) = do
  exp1 <- express l
  exp2 <- express r
  case exp1 of
    Number -> do return()
    Boolean -> do errUnexpectedType l Boolean Number
    Void -> do errVoidExp l
  case exp2 of
    Number -> do return()
    Boolean -> do errUnexpectedType r Boolean Number
    Void -> do errVoidExp r
  return Boolean


--Distinto de. Devuelve tipo Boolean y True si cumple la relación.
notEq :: Neq -> RetMonad Type
notEq (Neq l r) = do
  exp1 <- express l
  exp2 <- express r
  let eqType = exp1 == exp2
  case eqType of
    True -> do return()
    False -> do errExpectedEqual l r
  case exp1 of
    Number -> do return()
    Boolean -> do return()
    Void -> do errVoidExp l
  return Boolean


--Distinto de. Devuelve tipo Boolean y True si cumple la relación.
equal :: Equal -> RetMonad Type
equal (Equal l r) = do
  exp1 <- express l
  exp2 <- express r
  let eqType = exp1 == exp2
  case eqType of
    True -> do return()
    False -> do errExpectedEqual l r
  case exp1 of
    Number -> do return()
    Boolean -> do return()
    Void -> do errVoidExp l
  return Boolean


--Negativo. Devuelve tipo Number y el valor.
negat :: Exp -> RetMonad Type
negat e = do
  var <- express e
  case var of
    Number -> do return()
    Boolean -> do errUnexpectedType e Boolean Number
    Void -> do errVoidExp e
  return Number


--Negación. Devuelve tipo Boolean y el valor.
nega :: Exp -> RetMonad Type
nega (ENot  e) = do
  var <- express e
  case var of
    Number -> do errUnexpectedType e Number Boolean
    Boolean -> do return()
    Void -> do errVoidExp e
  return Boolean


--Identificadores. Devuelve el tipo y el valor.
identifier :: Token -> RetMonad Type
identifier (TIdent p id) = do
  scope <- get
  let tableSym  = sym scope
  let var = findSym tableSym id
  case var of
    Nothing -> do errNotDeclared (TIdent p id)
    Just _ -> do return () 
  let x = fromJust var 
  return x


--Constante True. Devuelve tipo Boolean y True.
constTrue :: Token -> RetMonad Type
constTrue (TTrue _) = do
  return Boolean


--Constante False. Devuelve tipo Boolean y False.
constFalse :: Token -> RetMonad Type
constFalse (TFalse _) = do
  return Boolean


--Número. Devuelve tipo Number y el valor.
numb :: Token -> RetMonad Type
numb (TNum _ n) = do
  return Number




-- Funcion recursiva que verifica si los parametros coinciden en una
-- llamada a una funcion
checkPars :: Token -> [Type] -> [Exp] -> RetMonad ()
checkPars _ [] []         = do return ()                                -- Termina la verificacion con exito
checkPars f [] (_:_)      = do errArgNumber f                           -- ERROR, LLAMADA A FUNCION CON CANTIDAD DE PARAMETROS INCORRECTOS
checkPars f (_:_) []      = do errArgNumber f                           -- ERROR, LLAMADA A FUNCION CON CANTIDAD DE PARAMETROS INCORRECTOS    
checkPars f (ty:ts) (e:es) = do
  val <- express e                                                      -- calcular valor de la siguiente expresion
  case (ty /= val)  of                                              -- Verificar que coincida el tipo del siguiente parametro
    True  -> do errUnexpectedType e val ty                          -- ERROR NO COINCIDE TIPO DE PARAMETRO
    False -> checkPars f ts es                                          -- continuar verificando

-- Instruccion, llamada a una funcion
funcCall :: FCall -> RetMonad Type
funcCall (FCall (TIdent p id) exps) = do
  scope <- get
  let funcId = findFunc (func scope) id
  case funcId of
    Nothing   -> errFNotDeclared (TIdent p id)                      -- ERROR, FUNCION NO DECLARADA
    Just val  -> do                                                 -- La funcion si esta declarada
      checkPars (TIdent p id) (parameters val) (exps)               -- Verificar que los parametros coincidan en numero y tipo
      return (ret val)                           -- Retorna el valor de la funcion
