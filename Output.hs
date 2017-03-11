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


data Out = Out  { scp :: Scope
                , sub :: Bool
                }

instance Show Out where
  show  (Out (Scope (sym:xs) _ h c _ funName typeSc _) b) =
    ( (if h==0 then "\n" else "") ++ 
      sp ++ "Alcance " ++ counter ++ ": " ++ name ++ "\n" ++
      sp ++ tab ++ "Variables: " ++ syms ++ "\n" ++
      sp ++ tab ++ "Sub_alcances: " ++ (if b then "None" else "") ++ "\n")
    where sp  = (P.take (8*h) (repeat ' '))
          tab = (P.take (4) (repeat ' '))
          counter = (show_count c)
          name    = (show_name funName typeSc)
          syms    = if (M.null sym) then "None" else show_var $ M.toList sym
          show_count  (x:[]) = show x
          show_count  (x:xs) = (show_count xs) ++ "." ++ (show x)
          show_name   id  IsFun     = ('_':id)
          show_name   id  IsProgram = "_program"
          show_name   _   IsWithDo  = "_with_do"
          show_name   _   IsFor     = "_for"
          show_name   _   IsForBy   = "_for_by"
          show_var []               = ""
          show_var ((id, var):xs)   = "\n" ++ sp ++ tab ++ tab ++ id ++ " : " ++ show var ++ show_var xs

instance Show Variable where
  show (Variable Boolean _ _) = "boolean"
  show (Variable Number  _ _) = "number"
  
  
  
  
