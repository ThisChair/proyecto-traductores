module RunExpress where

import Control.Monad.RWS
import Tree
import TokenInfo
import Lexer
import RunMonad
import Data.Sequence as S
import Data.Map as M
import Data.Set as Set
import Prelude as P
import Data.Maybe
import RunError

--Maneja las expresiones
runExpress :: Exp -> RunMonad Variable
runExpress (ESum (Tree.Sum l r))  = addi (Tree.Sum l r)
runExpress (EDif (Dif l r))       = subs (Dif l r)
runExpress (EMul (Mul l r))       = multi (Mul l r)
runExpress (EDiv (Div l r))       = divi (Div l r)
runExpress (EMod (Mod l r))       = rest (Mod l r)
runExpress (EDivI (DivI l r))     = diviI (DivI l r)
runExpress (EModI (ModI l r))     = restI (ModI l r)
runExpress (EOr (Or l r))         = disy (Or l r)
runExpress (EAnd (And l r))       = conj (And l r)
runExpress (EGeq (Geq l r))       = greatEq (Geq l r)
runExpress (EGr (Gr l r))         = greater (Gr l r)
runExpress (ELeq (Leq l r))       = lessEq (Leq l r)
runExpress (ELess (Less l r))     = lesser (Less l r)
runExpress (ENeq (Neq l r))       = notEq (Neq l r)
runExpress (EEqual (Equal l r))   = equal (Equal l r)
runExpress (ENeg e)               = negat e
runExpress (ENot e)               = nega e
runExpress (EToken (TIdent p id)) = identifier (TIdent p id)
runExpress (EToken (TTrue p))     = constTrue (TTrue p)
runExpress (EToken (TFalse p))    = constFalse (TFalse p)
runExpress (EToken (TNum p n))    = numb (TNum p n)
runExpress (EFCall (FCall t exp)) = funcCall (FCall t exp)


--Sumas. Devuelve tipo Number y la suma de los valores.
addi :: Tree.Sum -> RunMonad Variable
addi (Tree.Sum l r) = do
  exp1 <- runExpress l
  exp2 <- runExpress r
  let val = num exp1 + num exp2
  return (Variable Number val False True)

  
--Restas. Devuelve tipo Number y la resta de los valores.
subs :: Dif -> RunMonad Variable
subs (Dif l r) = do
  exp1 <- runExpress l
  exp2 <- runExpress r
  let val = num exp1 - num exp2
  return (Variable Number val False True)


--Multiplicaciones. Devuelve tipo Number y el producto de los valores.
multi :: Mul -> RunMonad Variable 
multi (Mul l r) = do
  exp1 <- runExpress l
  exp2 <- runExpress r
  let val = num exp1 * num exp2
  return (Variable Number val False True)


--Divisiones. Devuelve tipo Number y el cociente de los valores.
divi :: Div -> RunMonad Variable
divi (Div l r) = do
  exp1 <- runExpress l
  exp2 <- runExpress r
  case num exp2 of
    0 -> do errDivZero r
    _ -> do return()
  let val = num exp1 / num exp2
  return (Variable Number val False True)


--Resto. Devuelve tipo Number y el resto de la división de los valores.
rest :: Mod -> RunMonad Variable
rest (Mod l r) = do
  exp1 <- runExpress l
  exp2 <- runExpress r
  case num exp2 of
    0 -> do errDivZero r
    _ -> do return()
  let dob1 = num exp1
  let dob2 = num exp2
  let int1 = floor dob1
  let int2 = floor dob2
  let val = fromInteger (int1 `mod` int2) -- Esto no es la operación que piden.
  return (Variable Number val False True)


--División entera. Devuelve tipo Number y la división entera de los valores.
diviI :: DivI -> RunMonad Variable
diviI (DivI l r) = do
  exp1 <- runExpress l
  exp2 <- runExpress r
  case num exp2 of
    0 -> do errDivZero r
    _ -> do return()
  let dob1 = num exp1
  let dob2 = num exp2
  let int1 = floor dob1
  let int2 = floor dob2
  let val = fromInteger (int1 `div` int2)
  return (Variable Number val False True)


--Resto entero. Devuelve tipo Number y el resto de la división de los valores.
restI :: ModI -> RunMonad Variable
restI (ModI l r) = do
  exp1 <- runExpress l
  exp2 <- runExpress r
  case num exp2 of
    0 -> do errDivZero r
    _ -> do return()
  let dob1 = num exp1
  let dob2 = num exp2
  let int1 = round dob1
  let int2 = round dob2
  let val = fromInteger (int1 `mod` int2)
  return (Variable Number val False True)


--Disyunción. Devuelve tipo Boolean y la disyunción de los valores.
disy :: Or -> RunMonad Variable
disy (Or l r) = do
  exp1 <- runExpress l
  exp2 <- runExpress r
  let val = bool exp1 || bool exp2
  return (Variable Boolean 0 val True)


--Conjunción. Devuelve tipo Boolean y la conjunción de los valores.
conj :: And -> RunMonad Variable
conj (And l r) = do
  exp1 <- runExpress l
  exp2 <- runExpress r
  let val = bool exp1 && bool exp2
  return (Variable Boolean 0 val True)


--Mayor o igual que. Devuelve tipo Boolean y True si cumple la relación.
greatEq :: Geq -> RunMonad Variable
greatEq (Geq l r) = do
  exp1 <- runExpress l
  exp2 <- runExpress r
  let val = num exp1 >= num exp2
  return (Variable Boolean 0 val True)


--Mayor que. Devuelve tipo Boolean y True si cumple la relación.
greater :: Gr -> RunMonad Variable 
greater (Gr l r) = do
  exp1 <- runExpress l
  exp2 <- runExpress r
  let val = num exp1 > num exp2
  return (Variable Boolean 0 val True)


--Menor o igual que. Devuelve tipo Boolean y True si cumple la relación.
lessEq :: Leq -> RunMonad Variable
lessEq (Leq l r) = do
  exp1 <- runExpress l
  exp2 <- runExpress r
  let val = num exp1 <= num exp2
  return (Variable Boolean 0 val True)


--Menor que. Devuelve tipo Boolean y True si cumple la relación.
lesser :: Less -> RunMonad Variable
lesser (Less l r) = do
  exp1 <- runExpress l
  exp2 <- runExpress r
  let val = num exp1 < num exp2
  return (Variable Boolean 0 val True)


--Distinto de. Devuelve tipo Boolean y True si cumple la relación.
notEq :: Neq -> RunMonad Variable
notEq (Neq l r) = do
  exp1 <- runExpress l
  exp2 <- runExpress r
  let val = case t exp1 of 
              Number -> num exp1 /= num exp2
              Boolean -> bool exp1 /= bool exp2
  return (Variable Boolean 0 val True)


--Distinto de. Devuelve tipo Boolean y True si cumple la relación.
equal :: Equal -> RunMonad Variable
equal (Equal l r) = do
  exp1 <- runExpress l
  exp2 <- runExpress r
  let val = case t exp1 of 
              Number -> num exp1 == num exp2
              Boolean -> bool exp1 == bool exp2
  return (Variable Boolean 0 val True)


--Negativo. Devuelve tipo Number y el valor.
negat :: Exp -> RunMonad Variable
negat e = do
  var <- runExpress e
  let val = - num var
  return (Variable Number val False True)


--Negación. Devuelve tipo Boolean y el valor.
nega :: Exp -> RunMonad Variable
nega (ENot  e) = do
  var <- runExpress e
  let val = not $ bool var
  return (Variable Boolean 0 val True)


--Identificadores. Devuelve el tipo y el valor.
identifier :: Token -> RunMonad Variable
identifier (TIdent p id) = do
  scope <- get
  let tableSym  = sym scope
  let var = findSym tableSym id
  let x = fromJust var 
  return x


--Constante True. Devuelve tipo Boolean y True.
constTrue :: Token -> RunMonad Variable
constTrue (TTrue _) = do
  return (Variable Boolean 0 True False)


--Constante False. Devuelve tipo Boolean y False.
constFalse :: Token -> RunMonad Variable
constFalse (TFalse _) = do
  return (Variable Boolean 0 False False)


--Número. Devuelve tipo Number y el valor.
numb :: Token -> RunMonad Variable
numb (TNum _ n) = do
  return (Variable Number n False False)

--Función para añadir los parámetros y sus valores al alcance.
addPars :: [String] -> [Exp] -> RunMonad Variable
addPars [] [] = do return nullVariable
addPars (id:ids) (e:es) = do
  val <- runExpress e
  scope <- get
  case (t val) of
    Number -> do 
      modify(insertSym id (Variable Number (num val) False True))
      a <- addPars ids es
      return a
    Boolean -> do 
      modify(insertSym id (Variable Boolean 0 (bool val) True))
      a <- addPars ids es
      return a

-- Instruccion, llamada a una funcion
funcCall :: FCall -> RunMonad Variable
funcCall (FCall (TIdent p id) exps) = do
  scope <- get
  let funcId = findFunc (func scope) id
  let val = fromJust funcId
  modify(changeName id)                                               -- Cambiar el nombre del identificador de la funcion
  modify(changeTypeScope IsFun)                                       -- Cambiar el tipo de alcance
  modify(modifyTable  addTable)                                       -- Añadir una nueva tabla de simbolos
  a <- addPars (paramId val) exps                                     -- Agregar los parametros en la tabla de simbolos
  modify(changeFoundR Nothing)                                        -- No se ha encontrado valor de retorno.
  P.mapM_ instruction (instructions val)
  scopeR <- get
  case (foundR scopeR) of
    Nothing -> do return () -- Error ):
    Just x -> do
      modify(modifyTable eraseLastScope)
      return x
