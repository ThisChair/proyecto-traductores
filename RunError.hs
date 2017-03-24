module RunError where

import Control.Monad.RWS
import Tree
import TokenInfo
import Lexer
import RunMonad
import Data.Sequence as S
import Data.Map as M
import Data.Set as Set
import Prelude as P
import ContextError

--Error relativo a la división/resto entre cero.
errDivZero :: Exp -> a
errDivZero exp = error $ "Error en la fila " ++ show l ++ " cerca de la columna " ++ show c ++ ": La expresión '" ++ show_exp exp ++ "' evalúa a 0, pero fue usado como operador derecho en una división/resto."
                    where (l,c) = retrievePos exp

--Error relativo a tratar de modificar una variable inmutable.
errInmutable :: Token -> a
errInmutable (TIdent (AlexPn _ l c) id) = error $ "Error en la fila " ++ show l ++ " cerca de la columna " ++ show c ++ ": La variable '" ++ id ++ "' es inmutable, pero intentó modificarse."

--Error relativo a la lectura incorrecta en instrucción Read.
errRead :: Token -> Type -> a
errRead (TIdent (AlexPn _ l c) id) t = error $ "Error en la fila " ++ show l ++ " cerca de la columna " ++ show c ++ ": Lectura incorrecta para la variable '" ++ id ++ "' de tipo " ++ show t ++ " en instrucción read."

--Error relativo a no llegar a un valor de retorno en una función que lo espera.
errNoRet :: Token -> a
errNoRet (TIdent (AlexPn _ l c) id) = error $ "Error en la fila " ++ show l ++ " cerca de la columna " ++ show c ++ ": Se esperaba un valor de retorno, pero la función '" ++ id ++ "' no devuelve ninguno."