module ContextError where

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

--Obtener fila y columna en la que empieza una expresión.
retrievePos :: Exp -> (Int,Int)
retrievePos (ESum (Tree.Sum l r))                        = retrievePos l
retrievePos (EDif (Dif l r))                             = retrievePos l
retrievePos (EMul (Mul l r))                             = retrievePos l
retrievePos (EDiv (Div l r))                             = retrievePos l
retrievePos (EMod (Mod l r))                             = retrievePos l
retrievePos (EDivI (DivI l r))                           = retrievePos l
retrievePos (EModI (ModI l r))                           = retrievePos l
retrievePos (EOr (Or l r))                               = retrievePos l
retrievePos (EAnd (And l r))                             = retrievePos l
retrievePos (EGeq (Geq l r))                             = retrievePos l
retrievePos (EGr (Gr l r))                               = retrievePos l
retrievePos (ELeq (Leq l r))                             = retrievePos l
retrievePos (ELess (Less l r))                           = retrievePos l
retrievePos (ENeq (Neq l r))                             = retrievePos l
retrievePos (EEqual (Equal l r))                         = retrievePos l
retrievePos (ENeg e)                                     = retrievePos e
retrievePos (ENot e)                                     = retrievePos e
retrievePos (EToken (TIdent (AlexPn _ l c) _))           = (l,c)
retrievePos (EToken (TTrue (AlexPn _ l c)))              = (l,c)
retrievePos (EToken (TFalse (AlexPn _ l c)))             = (l,c)
retrievePos (EToken (TNum (AlexPn _ l c) _))             = (l,c)
retrievePos (EFCall (FCall (TIdent (AlexPn _ l c) _) _)) = (l,c)

--String representando la expresión.
show_exp :: Exp -> String
show_exp (ESum (Tree.Sum l r))          = show_exp l ++ " + " ++ show_exp r
show_exp (EDif (Dif l r))               = show_exp l ++ " - " ++ show_exp r
show_exp (EMul (Mul l r))               = show_exp l ++ " * " ++ show_exp r
show_exp (EDiv (Div l r))               = show_exp l ++ " / " ++ show_exp r
show_exp (EMod (Mod l r))               = show_exp l ++ " % " ++ show_exp r
show_exp (EDivI (DivI l r))             = show_exp l ++ " div " ++ show_exp r
show_exp (EModI (ModI l r))             = show_exp l ++ " mod " ++ show_exp r
show_exp (EOr (Or l r))                 = show_exp l ++ " or " ++ show_exp r
show_exp (EAnd (And l r))               = show_exp l ++ " and " ++ show_exp r
show_exp (EGeq (Geq l r))               = show_exp l ++ " >= " ++ show_exp r
show_exp (EGr (Gr l r))                 = show_exp l ++ " > " ++ show_exp r
show_exp (ELeq (Leq l r))               = show_exp l ++ " <= " ++ show_exp r
show_exp (ELess (Less l r))             = show_exp l ++ " < " ++ show_exp r
show_exp (ENeq (Neq l r))               = show_exp l ++ " /= " ++ show_exp r
show_exp (EEqual (Equal l r))           = show_exp l ++ " == " ++ show_exp r
show_exp (ENeg e)                       = "- " ++ show_exp e
show_exp (ENot e)                       = "not " ++ show_exp e
show_exp (EToken (TIdent _ id))         = id
show_exp (EToken (TTrue _))             = "true"
show_exp (EToken (TFalse _))            = "false"
show_exp (EToken (TNum _ n))            = show n
show_exp (EFCall (FCall (TIdent _ id) _)) = id ++ "()"

--Función que determina si un identificador aparece en una expresión.
inExp :: Token -> Exp -> Bool
inExp t (ESum (Tree.Sum l r))                = inExp t l || inExp t r
inExp t (EDif (Dif l r))                     = inExp t l || inExp t r
inExp t (EMul (Mul l r))                     = inExp t l || inExp t r
inExp t (EDiv (Div l r))                     = inExp t l || inExp t r
inExp t (EMod (Mod l r))                     = inExp t l || inExp t r
inExp t (EDivI (DivI l r))                   = inExp t l || inExp t r
inExp t (EModI (ModI l r))                   = inExp t l || inExp t r
inExp t (EOr (Or l r))                       = inExp t l || inExp t r
inExp t (EAnd (And l r))                     = inExp t l || inExp t r
inExp t (EGeq (Geq l r))                     = inExp t l || inExp t r
inExp t (EGr (Gr l r))                       = inExp t l || inExp t r
inExp t (ELeq (Leq l r))                     = inExp t l || inExp t r
inExp t (ELess (Less l r))                   = inExp t l || inExp t r
inExp t (ENeq (Neq l r))                     = inExp t l || inExp t r
inExp t (EEqual (Equal l r))                 = inExp t l || inExp t r
inExp t (ENeg e)                             = inExp t e
inExp t (ENot e)                             = inExp t e
inExp (TIdent _ id1) (EToken (TIdent _ id2)) = id1 == id2
inExp _ (EToken (TTrue _))                   = False
inExp _ (EToken (TFalse _))                  = False
inExp _ (EToken (TNum _ _))                  = False
inExp t (EFCall (FCall _ e))                 = or $ P.map (inExp t) e

--Error relativo a usar una variable en la asignación de si misma.
errRecAssig :: Token -> a
errRecAssig(TIdent (AlexPn _ l c) id) = error $ "Error en la fila " ++ show l ++ ", columna " ++ show c ++ ": Variable '" ++ id ++ "' siendo llamada durante su declaración."

--Error relativo a usar una variable de ciclo en sus guardias.
errForVar :: Token -> Exp -> a
errForVar(TIdent (AlexPn _ l c) id) e = error $ "Error en la fila " ++ show l ++ ", columna " ++ show c ++ ": Variable de ciclo '" ++ id ++ "' siendo llamada en la expresión de guardia '" ++ show_exp  e ++"'."

--Error relativo a esperar un tipo y obtener otro distinto.
errUnexpectedType :: Exp -> Type -> Type -> a
-- e tiene tipo obtT y se esperaba tipo expT
errUnexpectedType e obtT expT = error $ "Error en la fila " ++ show l ++ " cerca de la columna " ++ show c ++ ": '" ++ show_exp e ++ "' es una expresión de tipo '" ++ show obtT ++ "' y se esperaba una de tipo '" ++ show expT ++ "'."
                            where (l,c) = retrievePos e

--Error relativo a esperar tipos iguales en un operador binario pero obtener dos distintos.
errExpectedEqual :: Exp -> Exp -> a
-- leftE tiene un tipo y rightE tiene otro distinto
errExpectedEqual leftE rightE = error $ "Error en la fila " ++ show l ++ " cerca de la columna " ++ show c ++ ": Se esperaba que las expresiones '" ++ show_exp leftE ++ "' y '" ++ show_exp rightE ++ "' tuvieran el mismo tipo."
                    where (l,c) = retrievePos leftE

--Error relativo a usar una variable no declarada.
errNotDeclared :: Token -> a
errNotDeclared (TIdent (AlexPn _ l c) id) = error $ "Error en la fila " ++ show l ++ ", columna " ++ show c ++ ": Variable '" ++ id ++ "' no declarada."


--Error relativo a usar una función no declarada.
errFNotDeclared :: Token -> a
errFNotDeclared (TIdent (AlexPn _ l c) id) = error $ "Error en la fila " ++ show l ++ ", columna " ++ show c ++ ": Función '" ++ id ++ "' no declarada."


--Error relativo al pasaje de un número incorrecto de parámetros.
errArgNumber :: Token -> a
errArgNumber (TIdent (AlexPn _ l c) id) = error $ "Error en la fila " ++ show l ++ " cerca de la columna " ++ show c ++ ": Número incorrecto de parámetros en función '" ++ id ++ "'."


--Error relativo a que la función ya fue declarada.
errFDeclared :: DefFunc -> a
errFDeclared (DFun (TIdent (AlexPn _ l c) id ) _ _) = error $ "Error en la fila " ++ show l ++ ", columna " ++ show c ++ ": La función '" ++ id ++ "' ya fue declarada."
errFDeclared (DFunR (TIdent (AlexPn _ l c) id ) _ _ _) = error $ "Error en la fila " ++ show l ++ " cerca de la columna " ++ show c ++ ": La función '" ++ id ++ "' ya fue declarada."


--Error relativo a que la función usa parámetros con nombres iguales.
errRepArg :: DefFunc -> a
errRepArg (DFun (TIdent (AlexPn _ l c) id ) _ _) = error $ "Error en la fila " ++ show l ++ " cerca de la columna " ++ show c ++ ": La función '" ++ id ++ "' usa parámetros con nombres repetidos."
errRepArg (DFunR (TIdent (AlexPn _ l c) id ) _ _ _) = error $ "Error en la fila " ++ show l ++ " cerca de la columna " ++ show c ++ ": La función '" ++ id ++ "' usa parámetros con nombres repetidos."

--Error relativo a que la variable ya fue declarada.
errDeclared :: Token -> a
errDeclared (TIdent (AlexPn _ l c) id ) = error $ "Error en la fila " ++ show l ++ ", columna " ++ show c ++ ": La variable '" ++ id ++ "' ya fue declarada."

--Error relativo a encontrar un return donde no se espera uno.
errUnexpectedReturn :: Exp -> a
errUnexpectedReturn e = error $ "Error en la fila " ++ show l ++ " cerca de la columna " ++ show c ++ ": Se encontró un 'return' en una función sin valor de retorno."
                            where (l,c) = retrievePos e