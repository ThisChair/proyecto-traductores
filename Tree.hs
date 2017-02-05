module Main(main) where
import Data.Tree
import System.IO
import System.Environment


-- Es necesario definir los diferentes tipos del arbol
-- Es necesario definir una funcion que indique que se imprimira
-- por cada tipo en el arbol
data TypeNode = Funs    |
                Is      |
                S       |
                Exp     |
                AritE   |
                BoolE   |
                AComp   |
                BComp   |
                Type    |
                Dec     |
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
                Ins
                deriving (Show)





children :: Tree a -> Forest a
children (Node x f) = f

getType :: (Show a) => Tree a -> String
getType (Node x _) = show x
 
 
 
main = printTree 0 (Node Is [Node For [], Node ForBy [], Node Is[]])

printTree :: Int -> Tree TypeNode -> IO()
printTree h tree = do
  let tab = take (3*h) (repeat ' ')
  putStrLn $ tab ++ (getType tree)
  let child = children tree
  case (length child == 0) of
      False  ->  do  mapM_  (printTree (h+1))  (children tree)
      True   ->  do  return ()
      

    

