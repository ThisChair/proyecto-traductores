module Tree where
import Data.Tree
import Lexer
import System.IO
import System.Environment


-- Es necesario definir los diferentes tipos del arbol
-- Los nodos terminales seran los tokens en si, solo hace
-- falta definir tipos de datos para nodos NO terminales
-- Es necesario definir una funcion que indique que se imprimira
-- por cada tipo en el arbol
data TypeNode = Init    |
                Funs    |
                Is      |
                S       |
                Exp     |
                AritE   |
                BoolE   |
                AComp   |
                BComp   |
                Type    |
                Dec     |
                Ds      |
                Ids     |
                Assig   |
                Read    |
                Write   |
                WriteL  |
                Print   |
                Prints  |
                DefFunc |
                DFun    |
                DFunR   |
                Pars    |
                Ps      |
                Par     |
                Block   |
                Do      |
                If      |
                IfElse  |
                While   |
                For     |
                ForBy   |
                Repeat  |
                Ins     |
                IsToken Token 
                deriving (Show)





children :: Tree a -> Forest a
children (Node x f) = f

getType :: (Show a) => Tree a -> String
getType (Node x _) = show x
 
 
 
-- main = printTree 0 (Node (IsToken (TFalse (AlexPn 0 0 0))) [Node For [], Node ForBy [], Node Is[]])

printTree :: Int -> Tree TypeNode -> IO()
printTree h tree = do
  let tab = take (3*h) (repeat ' ')
  putStrLn $ tab ++ (getType tree)
  let child = children tree
  case (length child == 0) of
      False  ->  do  mapM_  (printTree (h+1))  (children tree)
      True   ->  do  return ()
      



 

