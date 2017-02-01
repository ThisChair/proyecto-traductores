{
module Main(main) where
import System.IO
import System.Environment
import Data.Char
}

%wrapper "posn"

$symbol = [\=\+\-\*\/\(\)\<\>\%\,\;]  -- simple symbols
$nothing = ~[]

tokens :-
  $white+               ;
  \#.*\n                ;
  (program | read | writeln | write | end | with | do | if | then | else
  while for from)       { (\p s -> Reserved p s) }
  (not | and | or)      { (\p s -> Reserved p s) }
  (div | mod)           { (\p s -> Reserved p s) }
  read                  { (\p s -> Reserved p s) }
  writeln               { (\p s -> Reserved p s) }
  write                 { (\p s -> Reserved p s) }
  end                   { (\p s -> Reserved p s) }
  with                  { (\p s -> Reserved p s) }
  do                    { (\p s -> Reserved p s) }
  if                    { (\p s -> Reserved p s) }
  then                  { (\p s -> Reserved p s) }
  else                  { (\p s -> Reserved p s) }
  while                 { (\p s -> Reserved p s) }
  for                   { (\p s -> Reserved p s) }
  from                  { (\p s -> Reserved p s) }
  to                    { (\p s -> Reserved p s) }
  repeat                { (\p s -> Reserved p s) }
  times                 { (\p s -> Reserved p s) }
  func                  { (\p s -> Reserved p s) }
  $symbol               { (\p s -> Sym p s) }
  \/\=                  { (\p s -> Sym p s) }
  \=\=                  { (\p s -> Sym p s) }
  \>\=                  { (\p s -> Sym p s) }
  \<\=                  { (\p s -> Sym p s) }
  \-\>                  { (\p s -> Sym p s) }
  boolean               { (\p s -> Type p s) }
  number                { (\p s -> Type p s) }
  true                  { (\p s -> Bool p s) }
  false                 { (\p s -> Bool p s) }
  [a-z][a-zA-Z0-9_]*    { (\p s -> Ident p s) }
  [0-9]+(\.[0-9]+)*     { (\p s -> Num p (read s)) }
  \"$printable*\"       { (\p s -> Str p s) }
  $nothing              { (\p s -> Undef p s) }


{
-- The token type:
data Token =
    Reserved    AlexPosn String     |
    Type        AlexPosn String     |
    Ident       AlexPosn String     |
    Bool        AlexPosn String     |
    Num         AlexPosn Double     |
    Str         AlexPosn String     |     
    Sym         AlexPosn String     |
    Undef       AlexPosn String 
    deriving (Eq,Show)



show_type :: Token -> String
show_type (Reserved p s)    = "palabra reservada"
show_type (Type p s)        = "tipo de dato"
show_type (Ident p s)       = "identificador"
show_type (Bool p s)        = "literal booleano"
show_type (Num p s)         = "literal numérico"
show_type (Str p s)         = "literal string"
show_type (Sym p s)         = "signo"
show_type (Undef p s)       = "caracter inesperado"

show_pos :: Token -> String
show_pos (Reserved  (AlexPn _ i j) s) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (Type      (AlexPn _ i j) s) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (Ident     (AlexPn _ i j) s) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (Bool      (AlexPn _ i j) s) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (Num       (AlexPn _ i j) s) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (Str       (AlexPn _ i j) s) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (Sym       (AlexPn _ i j) s) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (Undef     (AlexPn _ i j) s) = "linea " ++ show i ++ ", columna " ++ show j


show_val :: Token -> String
show_val (Reserved p s) = s
show_val (Type p s)     = s
show_val (Ident p s)    = s
show_val (Bool p s)     = s
show_val (Num p s)      = show s
show_val (Str p s)      = s
show_val (Sym p s)      = s
show_val (Undef p s)    = s


show_token:: Token -> String
show_token tok = show_pos tok ++ ": " ++ show_type tok ++ " '" ++ show_val tok ++ "'"

undef :: Token -> Bool
undef (Undef p s)       = True
undef (Reserved p s)    = False
undef (Type p s)        = False
undef (Ident p s)       = False
undef (Bool p s)        = False
undef (Num p s)         = False
undef (Str p s)         = False
undef (Sym p s)         = False


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
  let ans = if inv == [] then toks else inv
  mapM_ putStrLn $ map show_token ans
}
