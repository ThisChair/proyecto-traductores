-- Carlos Infante 13-10681
-- Rubmary Rojas 13-11264
-- Programa principal, analisis lexicografico y sintactico

module Main(main) where
import Lexer
import System.IO
import System.Environment
import Data.Char
import Parser
import TokenInfo
import Tree
import Control.Monad.State.Strict
import Control.Monad.RWS.Strict
import Prelude as P
import RetMonad
import Funciones
import Data.Sequence as S
import Data.Foldable as F
import Control.DeepSeq
import Control.Exception
import RunMonad as RM
import RunFunciones as RF

-- Obtener archivo con el formato correcto, si no es el archivo correcto
-- se obtiene un error
filePath :: [String] -> String
filePath [] = error "No se introdujo un archivo."
filePath (x:y:_) = error "Introduzca un solo argumento."
filePath (x:_) = case P.reverse x of  ('n':'t':'r':'.':_) -> x
                                      (y:_) -> error "Formato de archivo incorrecto."

-- Funcion principal
-- Abre el archivo, procesa los tokens, si hay alguno incorrecto, muestra todos los
-- tokens incorrectos con su posicion. Si todos los tokens son correctos, analiza la
-- sintaxis, si es correcta imprime el arbol, en caso contrario se obtiene un mensaje
-- de error


main::IO ()
main = do
  args <- getArgs
  handle <- openFile (filePath args) ReadMode  
  s <- hGetContents handle  
  let toks = alexScanTokens s
  let inv =  P.filter undef toks
  let val = (inv == [])
  case val of
      False -> do P.mapM_ putStrLn $ P.map show_token inv
      True  -> do 
                let parse   = parseRet toks
                let context = evalState (start parse) initialState
                fs <- evaluate $ force context
                let (s, w) = execRWS (RF.start parse) "" RM.initialState
                putStr $ "Nice"
      
