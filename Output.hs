-- Funciones para imprimir


module Output where
import Control.Monad.RWS
import Tree
import TokenInfo
import Lexer
import Parser
import Data.Sequence as S
import Data.Map as M
import Data.Set as Set
import Prelude  as P
import Data.Maybe
import RetMonad
import Express



instance Show Scope where
  show (Scope sym _ h c _ funName typeSc) = 
    sp ++ "Alcance " ++ counter ++ ": " ++ name ++ "\n" ++ sp ++ "Variables: " ++ syms ++ "\n"
    where sp = (P.take (4*h) (repeat ' '))
          counter = (show_count c)
          name    = (show_name funName typeSc)
          syms    = if (P.length sym == 0) then "None" else "\n" ++ sp ++ (show $ M.toList $ head sym)
          show_count  (x:[]) = show x
          show_count  (x:xs) = (show_count xs) ++ "." ++ (show x)
          show_name   id  IsFun     = ('_':id)
          show_name   id  IsProgram = "_program"
          show_name   _   IsWithDo  = "_withdo"
          show_name   _   IsFor     = "_for"
          show_name   _   IsForBy   = "_forby"
--          show_sym []                = []
--          show_sym (x:xs)            = show_var 
