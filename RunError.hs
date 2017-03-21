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