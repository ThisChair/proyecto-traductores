module Tree where
import Data.Tree
import Lexer
import System.IO
import System.Environment
import TokenInfo


-- Es necesario definir los diferentes tipos del arbol
-- Los nodos terminales seran los tokens en si, solo hace
-- falta definir tipos de datos para nodos NO terminales
-- Es necesario definir una funcion que indique que se imprimira
-- por cada tipo en el arbol
data TypeNode = Init    |
                Funs    |
                Is      |
                Exp     |
                Type    |
                Dec     |
                Ds      |
                Assig   |
                Read    |
                Write   |
                WriteL  |
                Prints  |
                DFun    |
                DFunR   |
                FBody   |
                Pars    |
                Ps      |
                Par     |
                Do      |
                If      |
                IfElse  |
                While   |
                For     |
                ForBy   |
                Repeat  |
                ExpS    |
                FCall   |
                Ret     |
                IsToken Token 

instance Show TypeNode where
    show Init    = "Inicio de programa:"
    show Funs    = "Funciones:"
    show Is      = "Instrucciones:"
    show Exp     = "Expresión:"
    show Dec     = "Declaración:"
    show Ds      = "Declaraciones:"
    show Assig   = "Asignación:"
    show Read    = "Leer entrada:"
    show Write   = "Salida:"
    show WriteL  = "Salida con salto:"
    show Prints  = "Strings:"
    show DFun    = "Función:"
    show DFunR   = "Función con tipo de retorno:"
    show FBody   = "Cuerpo de la función:"
    show Pars    = "Argumentos:"
    show Par     = "Argumento:"
    show Do      = "With Do:"
    show If      = "If:"
    show IfElse  = "If Else:"
    show While   = "While:"
    show For     = "For:"
    show ForBy   = "For By:"
    show Repeat  = "Repeat:"
    show ExpS    = "Expresiones:"
    show FCall   = "Llamada a función:"
    show Ret     = "Retorno de función:"
    show (IsToken t) = show_val t


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
      

