module Main(main) where
import Lexer
import System.IO
import System.Environment
import Data.Char
import Parser
import TokenInfo
import Tree

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
                printTree 0 parse
