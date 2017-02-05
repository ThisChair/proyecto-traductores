{
module Main(main) where
import System.IO
import System.Environment
import Data.Char
}

%wrapper "posn"

$print  = $printable # [\\\"]
$nothing = ~[]
tokens :-
    $white+                 ;
    \#.*\n                  ;
    program                 { (\p s -> TProgram p ) }
    read                    { (\p s -> TRead p ) }
    writeln                 { (\p s -> TWriteLn p ) }
    write                   { (\p s -> TWrite p ) }
    end                     { (\p s -> TEnd p ) }
    with                    { (\p s -> TWith p) }
    do                      { (\p s -> TDo p ) }
    if                      { (\p s -> TIf p ) }
    then                    { (\p s -> TThen p ) }
    else                    { (\p s -> TElse p ) }
    while                   { (\p s -> TWhile p ) }
    for                     { (\p s -> TFor p ) }
    from                    { (\p s -> TFrom p ) }
    to                      { (\p s -> TTo p ) }
    repeat                  { (\p s -> TRepeat p ) }
    times                   { (\p s -> TTimes p ) }
    func                    { (\p s -> TFunc p ) }
    not                     { (\p s -> TNot p ) }
    and                     { (\p s -> TAnd p ) }
    or                      { (\p s -> TOr p ) }
    div                     { (\p s -> TDiv p ) }
    mod                     { (\p s -> TMod p ) }
    \/\=                    { (\p s -> TNotEq p ) }
    \=\=                    { (\p s -> TEq p ) }
    \>\=                    { (\p s -> TMoreEq p ) }
    \<\=                    { (\p s -> TLessEq p ) }
    \-\>                    { (\p s -> TReturn p ) }
    \=                      { (\p s -> TAssign p ) }
    \+                      { (\p s -> TPlus p ) }
    \-                      { (\p s -> TMinus p ) }
    \*                      { (\p s -> TStar p ) }
    \/                      { (\p s -> TSlash p ) }
    \(                      { (\p s -> TOpenP p ) }
    \)                      { (\p s -> TCloseP p ) }
    \<                      { (\p s -> TLess p ) }
    \>                      { (\p s -> TMore p ) }
    \%                      { (\p s -> TPercent p ) }
    \,                      { (\p s -> TComma p ) }
    \;                      { (\p s -> TSColon p ) }
    boolean                 { (\p s -> TBoolean p ) }
    number                  { (\p s -> TNumber p ) }
    true                    { (\p s -> TTrue p ) }
    false                   { (\p s -> TFalse p ) }
    [a-z][a-zA-Z0-9_]*      { (\p s -> TIdent p s) }
    [0-9]+(\.[0-9]+)*       { (\p s -> TNum p (read s)) }
    \"($print | (\\\\) | (\\n) | (\\\"))*\"       
                            { (\p s -> TString p s) }
    $nothing                {TUndef}
{
-- The token type:
data Token =
    TProgram    AlexPosn            |
    TRead       AlexPosn            |
    TWriteLn    AlexPosn            |
    TWrite      AlexPosn            |
    TEnd        AlexPosn            |
    TWith       AlexPosn            |
    TDo         AlexPosn            |
    TIf         AlexPosn            |
    TThen       AlexPosn            |
    TElse       AlexPosn            |
    TWhile      AlexPosn            |
    TFor        AlexPosn            |
    TFrom       AlexPosn            |
    TTo         AlexPosn            |
    TRepeat     AlexPosn            |
    TTimes      AlexPosn            |
    TFunc       AlexPosn            |
    TNot        AlexPosn            |
    TAnd        AlexPosn            |
    TOr         AlexPosn            |
    TDiv        AlexPosn            |
    TMod        AlexPosn            |
    TNotEq      AlexPosn            |
    TEq         AlexPosn            |
    TMoreEq     AlexPosn            |
    TLessEq     AlexPosn            |
    TReturn     AlexPosn            |
    TAssign     AlexPosn            |
    TPlus       AlexPosn            |
    TMinus      AlexPosn            |
    TStar       AlexPosn            |
    TSlash      AlexPosn            |
    TOpenP      AlexPosn            |
    TCloseP     AlexPosn            |
    TLess       AlexPosn            |
    TMore       AlexPosn            |
    TPercent    AlexPosn            |
    TComma      AlexPosn            |
    TSColon     AlexPosn            |
    TBoolean    AlexPosn            |
    TNumber     AlexPosn            |
    TTrue       AlexPosn            |
    TFalse      AlexPosn            |
    TIdent      AlexPosn String     |
    TNum        AlexPosn Double     |
    TString     AlexPosn String     |
    TUndef      AlexPosn String
    deriving (Eq,Show)
    
show_type :: Token -> String
show_type (TProgram p) = "palabra reservada"
show_type (TRead p)    = "palabra reservada"
show_type (TWriteLn p) = "palabra reservada"
show_type (TWrite p)   = "palabra reservada"
show_type (TEnd p)     = "palabra reservada"
show_type (TWith p)    = "palabra reservada"
show_type (TDo p)      = "palabra reservada"
show_type (TIf p)      = "palabra reservada"
show_type (TThen p)    = "palabra reservada"
show_type (TElse p)    = "palabra reservada"
show_type (TWhile p)   = "palabra reservada"
show_type (TFor p)     = "palabra reservada"
show_type (TFrom p)    = "palabra reservada"
show_type (TTo p)      = "palabra reservada"
show_type (TRepeat p)  = "palabra reservada"
show_type (TTimes p)   = "palabra reservada"
show_type (TFunc p)    = "palabra reservada"
show_type (TNot p)     = "palabra reservada"
show_type (TAnd p)     = "palabra reservada"
show_type (TOr p)      = "palabra reservada"
show_type (TDiv p)     = "palabra reservada"
show_type (TMod p)     = "palabra reservada"
show_type (TNotEq p)   = "signo"
show_type (TEq p)      = "signo"
show_type (TMoreEq p)  = "signo"
show_type (TLessEq p)  = "signo"
show_type (TReturn p)  = "signo"
show_type (TAssign p)  = "signo"
show_type (TPlus p)    = "signo"
show_type (TMinus p)   = "signo"
show_type (TStar p)    = "signo"
show_type (TSlash p)   = "signo"
show_type (TOpenP p)   = "signo"
show_type (TCloseP p)  = "signo"
show_type (TLess p)    = "signo"
show_type (TMore p)    = "signo"
show_type (TPercent p) = "signo"
show_type (TComma p)   = "signo"
show_type (TSColon p)  = "signo"
show_type (TBoolean p) = "tipo de dato"
show_type (TNumber p)  = "tipo de dato"
show_type (TTrue p)    = "literal booleano"
show_type (TFalse p)   = "literal booleano"
show_type (TIdent p s)   = "identificador"
show_type (TNum p s)     = "literal numérico"
show_type (TString p s)  = "literal string"
show_type (TUndef p s)   = "caracter inesperado"


show_pos :: Token -> String
show_pos (TProgram (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TRead (AlexPn _ i j))    = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TWriteLn (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TWrite (AlexPn _ i j))   = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TEnd (AlexPn _ i j))     = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TWith (AlexPn _ i j))    = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TDo (AlexPn _ i j))      = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TIf (AlexPn _ i j))      = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TThen (AlexPn _ i j) )    = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TElse (AlexPn _ i j) )    = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TWhile (AlexPn _ i j) )   = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TFor (AlexPn _ i j) )     = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TFrom (AlexPn _ i j) )    = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TTo (AlexPn _ i j) )      = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TRepeat (AlexPn _ i j) )  = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TTimes (AlexPn _ i j) )   = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TFunc (AlexPn _ i j) )    = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TNot (AlexPn _ i j) )     = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TAnd (AlexPn _ i j) )     = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TOr (AlexPn _ i j) )      = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TDiv (AlexPn _ i j) )     = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TMod (AlexPn _ i j) )     = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TNotEq (AlexPn _ i j))   = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TEq (AlexPn _ i j) )      = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TMoreEq (AlexPn _ i j) )  = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TLessEq (AlexPn _ i j) )  = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TReturn (AlexPn _ i j) )  = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TAssign (AlexPn _ i j) )  = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TPlus (AlexPn _ i j) )    = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TMinus (AlexPn _ i j) )   = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TStar (AlexPn _ i j) )    = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TSlash (AlexPn _ i j) )   = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TOpenP (AlexPn _ i j) )   = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TCloseP (AlexPn _ i j) )  = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TLess (AlexPn _ i j) )    = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TMore (AlexPn _ i j) )    = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TPercent (AlexPn _ i j) ) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TComma (AlexPn _ i j) )   = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TSColon (AlexPn _ i j) )  = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TBoolean (AlexPn _ i j) ) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TNumber (AlexPn _ i j) )  = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TTrue (AlexPn _ i j) )    = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TFalse (AlexPn _ i j) )   = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TIdent (AlexPn _ i j) s)   = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TNum (AlexPn _ i j) s)     = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TString (AlexPn _ i j) s)  = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TUndef (AlexPn _ i j) s)   = "linea " ++ show i ++ ", columna " ++ show j


show_val :: Token -> String
show_val (TProgram p)   = "program"
show_val (TRead p)      = "read"
show_val (TWriteLn p)   = "writeln"
show_val (TWrite p)     = "write"
show_val (TEnd p)       = "end"
show_val (TWith p)      = "with"
show_val (TDo p)        = "do"
show_val (TIf p)        = "if"
show_val (TThen p)      = "then"
show_val (TElse p)      = "else"
show_val (TWhile p)     = "while"
show_val (TFor p)       = "for"
show_val (TFrom p)      = "from"
show_val (TTo p)        = "to"
show_val (TRepeat p)    = "repeat"
show_val (TTimes p)     = "time"
show_val (TFunc p)      = "func"
show_val (TNot p)       = "not"
show_val (TAnd p)       = "and"
show_val (TOr p )       = "or"
show_val (TDiv p )      = "div"
show_val (TMod p )      = "mod"
show_val (TNotEq p)     = "\\="
show_val (TEq p )       = "=="
show_val (TMoreEq p)    = ">="
show_val (TLessEq p )   = "<="
show_val (TReturn p )   = "return"
show_val (TAssign p )   = "="
show_val (TPlus p )     = "+"
show_val (TMinus p )    = "-"
show_val (TStar p )     = "*"
show_val (TSlash p )    = "/"
show_val (TOpenP p )    = "("
show_val (TCloseP p )   = ")"
show_val (TLess p )     = "<"
show_val (TMore p )     = ">"
show_val (TPercent p )  = "%"
show_val (TComma p )    = ","
show_val (TSColon p )   = ";"
show_val (TBoolean p )  = "bool"
show_val (TNumber p )   = "number"
show_val (TTrue p )     = "true"
show_val (TFalse p )    = "false"
show_val (TIdent p s)   = s
show_val (TNum p s)     = show s
show_val (TString p s)  = s
show_val (TUndef p s)   = s
show_token:: Token -> String
show_token tok = show_pos tok ++ ": " ++ show_type tok ++ " '" ++ show_val tok ++ "'"


undef :: Token -> Bool
undef (TProgram p)    = False
undef (TRead p )      = False
undef (TWriteLn p )   = False
undef (TWrite p )     = False
undef (TEnd p )       = False
undef (TWith p )      = False
undef (TDo p )        = False
undef (TIf p )        = False
undef (TThen p )      = False
undef (TElse p )      = False
undef (TWhile p )     = False
undef (TFor p )       = False
undef (TFrom p )      = False
undef (TTo p )        = False
undef (TRepeat p )    = False
undef (TTimes p )     = False
undef (TFunc p )      = False
undef (TNot p )       = False
undef (TAnd p )       = False
undef (TOr p )        = False
undef (TDiv p )       = False
undef (TMod p )       = False
undef (TNotEq p )     = False
undef (TEq p )        = False
undef (TMoreEq p )    = False
undef (TLessEq p )    = False
undef (TReturn p )    = False
undef (TAssign p )    = False
undef (TPlus p )      = False
undef (TMinus p )     = False
undef (TStar p )      = False
undef (TSlash p )     = False
undef (TOpenP p )     = False
undef (TCloseP p )    = False
undef (TLess p )      = False
undef (TMore p )      = False
undef (TPercent p )   = False
undef (TComma p )     = False
undef (TSColon p)     = False
undef (TBoolean p )   =  False
undef (TNumber p )    = False
undef (TTrue p )      = False
undef (TFalse p )     = False
undef (TIdent p s)    = False
undef (TNum p s)      = False
undef (TString p s)   = False
undef (TUndef p s)    = True


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
