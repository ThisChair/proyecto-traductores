module Main(main) where
import Lexer
import System.IO
import System.Environment
import Data.Char
import Parse

show_pos :: Token -> String
show_pos (TUndef (AlexPn _ i j) s)   = "linea " ++ show i ++ ", columna " ++ show j


show_val :: Token -> String
show_val (TUndef p s)   = s

show_token:: Token -> String
show_token tok = show_pos tok ++ ": caracter inesperado" ++ " '" ++ show_val tok ++ "'"


undef :: Token -> Bool
undef (TUndef p s) = True
undef _ = False

filePath :: [String] -> String
filePath [] = error "No se introdujo un archivo."
filePath (x:y:_) = error "Introduzca un solo argumento."
filePath (x:_) = case reverse x of ('n':'t':'r':'.':_) -> x
                                   (y:_) -> error "Formato de archivo incorrecto."


main::IO ()
main = do
  args <- getArgs
  handle <- openFile (filePath args) ReadMode  
  s <- hGetContents handle  
  let toks = alexScanTokens s
  let inv =  filter undef toks
  let val = (inv == [])
  case val of
      False -> do mapM_ putStrLn $ map show_token inv
      True  -> do 
                let parse = parseRet toks
                putStrLn $ show parse
