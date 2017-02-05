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
    program                 { (\p s -> TProgram p s) }
    read                    { (\p s -> TRead p s) }
    writeln                 { (\p s -> TWriteLn p s) }
    write                   { (\p s -> TWrite p s) }
    end                     { (\p s -> TEnd p s) }
    with                    { (\p s -> TWith p s) }
    do                      { (\p s -> TDo p s) }
    if                      { (\p s -> TIf p s) }
    then                    { (\p s -> TThen p s) }
    else                    { (\p s -> TElse p s) }
    while                   { (\p s -> TWhile p s) }
    for                     { (\p s -> TFor p s) }
    from                    { (\p s -> TFrom p s) }
    to                      { (\p s -> TTo p s) }
    repeat                  { (\p s -> TRepeat p s) }
    times                   { (\p s -> TTimes p s) }
    func                    { (\p s -> TFunc p s) }
    not                     { (\p s -> TNot p s) }
    and                     { (\p s -> TAnd p s) }
    or                      { (\p s -> TOr p s) }
    div                     { (\p s -> TDiv p s) }
    mod                     { (\p s -> TMod p s) }
    \/\=                    { (\p s -> TNotEq p s) }
    \=\=                    { (\p s -> TEq p s) }
    \>\=                    { (\p s -> TMoreEq p s) }
    \<\=                    { (\p s -> TLessEq p s) }
    \-\>                    { (\p s -> TReturn p s) }
    \=                      { (\p s -> TAssign p s) }
    \+                      { (\p s -> TPlus p s) }
    \-                      { (\p s -> TMinus p s) }
    \*                      { (\p s -> TStar p s) }
    \/                      { (\p s -> TSlash p s) }
    \(                      { (\p s -> TOpenP p s) }
    \)                      { (\p s -> TCloseP p s) }
    \<                      { (\p s -> TLess p s) }
    \>                      { (\p s -> TMore p s) }
    \%                      { (\p s -> TPercent p s) }
    \,                      { (\p s -> TComma p s) }
    \;                      { (\p s -> TSColon p s) }
    boolean                 { (\p s -> TBoolean p s) }
    number                  { (\p s -> TNumber p s) }
    true                    { (\p s -> TTrue p s) }
    false                   { (\p s -> TFalse p s) }
    [a-z][a-zA-Z0-9_]*      { (\p s -> TIdent p s) }
    [0-9]+(\.[0-9]+)*       { (\p s -> TNum p (read s)) }
    \"($print | (\\\\) | (\\n) | (\\\"))*\"       
                            { (\p s -> TString p s) }
    $nothing                {TUndef}


{

-- The token type:
data Token =
    TProgram    AlexPosn String     |
    TRead       AlexPosn String     |
    TWriteLn    AlexPosn String     |
    TWrite      AlexPosn String     |
    TEnd        AlexPosn String     |
    TWith       AlexPosn String     |
    TDo         AlexPosn String     |
    TIf         AlexPosn String     |
    TThen       AlexPosn String     |
    TElse       AlexPosn String     |
    TWhile      AlexPosn String     |
    TFor        AlexPosn String     |
    TFrom       AlexPosn String     |
    TTo         AlexPosn String     |
    TRepeat     AlexPosn String     |
    TTimes      AlexPosn String     |
    TFunc       AlexPosn String     |
    TNot        AlexPosn String     |
    TAnd        AlexPosn String     |
    TOr         AlexPosn String     |
    TDiv        AlexPosn String     |
    TMod        AlexPosn String     |
    TNotEq      AlexPosn String     |
    TEq         AlexPosn String     |
    TMoreEq     AlexPosn String     |
    TLessEq     AlexPosn String     |
    TReturn     AlexPosn String     |
    TAssign     AlexPosn String     |
    TPlus       AlexPosn String     |
    TMinus      AlexPosn String     |
    TStar       AlexPosn String     |
    TSlash      AlexPosn String     |
    TOpenP      AlexPosn String     |
    TCloseP     AlexPosn String     |
    TLess       AlexPosn String     |
    TMore       AlexPosn String     |
    TPercent    AlexPosn String     |
    TComma      AlexPosn String     |
    TSColon     AlexPosn String     |
    TBoolean    AlexPosn String     |
    TNumber     AlexPosn String     |
    TTrue       AlexPosn String     |
    TFalse      AlexPosn String     |
    TIdent      AlexPosn String     |
    TNum        AlexPosn Double     |
    TString     AlexPosn String     |
    TUndef      AlexPosn String
    deriving (Eq,Show)
    
show_type :: Token -> String
show_type (TProgram p s) = "palabra reservada"
show_type (TRead p s)    = "palabra reservada"
show_type (TWriteLn p s) = "palabra reservada"
show_type (TWrite p s)   = "palabra reservada"
show_type (TEnd p s)     = "palabra reservada"
show_type (TWith p s)    = "palabra reservada"
show_type (TDo p s)      = "palabra reservada"
show_type (TIf p s)      = "palabra reservada"
show_type (TThen p s)    = "palabra reservada"
show_type (TElse p s)    = "palabra reservada"
show_type (TWhile p s)   = "palabra reservada"
show_type (TFor p s)     = "palabra reservada"
show_type (TFrom p s)    = "palabra reservada"
show_type (TTo p s)      = "palabra reservada"
show_type (TRepeat p s)  = "palabra reservada"
show_type (TTimes p s)   = "palabra reservada"
show_type (TFunc p s)    = "palabra reservada"
show_type (TNot p s)     = "palabra reservada"
show_type (TAnd p s)     = "palabra reservada"
show_type (TOr p s)      = "palabra reservada"
show_type (TDiv p s)     = "palabra reservada"
show_type (TMod p s)     = "palabra reservada"
show_type (TNotEq p s)   = "signo"
show_type (TEq p s)      = "signo"
show_type (TMoreEq p s)  = "signo"
show_type (TLessEq p s)  = "signo"
show_type (TReturn p s)  = "signo"
show_type (TAssign p s)  = "signo"
show_type (TPlus p s)    = "signo"
show_type (TMinus p s)   = "signo"
show_type (TStar p s)    = "signo"
show_type (TSlash p s)   = "signo"
show_type (TOpenP p s)   = "signo"
show_type (TCloseP p s)  = "signo"
show_type (TLess p s)    = "signo"
show_type (TMore p s)    = "signo"
show_type (TPercent p s) = "signo"
show_type (TComma p s)   = "signo"
show_type (TSColon p s)  = "signo"
show_type (TBoolean p s) = "tipo de dato"
show_type (TNumber p s)  = "tipo de dato"
show_type (TTrue p s)    = "literal booleano"
show_type (TFalse p s)   = "literal booleano"
show_type (TIdent p s)   = "identificador"
show_type (TNum p s)     = "literal numérico"
show_type (TString p s)  = "literal string"
show_type (TUndef p s)   = "caracter inesperado"

show_pos :: Token -> String
show_pos (TProgram (AlexPn _ i j) s) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TRead (AlexPn _ i j) s)    = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TWriteLn (AlexPn _ i j) s) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TWrite (AlexPn _ i j) s)   = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TEnd (AlexPn _ i j) s)     = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TWith (AlexPn _ i j) s)    = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TDo (AlexPn _ i j) s)      = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TIf (AlexPn _ i j) s)      = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TThen (AlexPn _ i j) s)    = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TElse (AlexPn _ i j) s)    = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TWhile (AlexPn _ i j) s)   = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TFor (AlexPn _ i j) s)     = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TFrom (AlexPn _ i j) s)    = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TTo (AlexPn _ i j) s)      = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TRepeat (AlexPn _ i j) s)  = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TTimes (AlexPn _ i j) s)   = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TFunc (AlexPn _ i j) s)    = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TNot (AlexPn _ i j) s)     = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TAnd (AlexPn _ i j) s)     = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TOr (AlexPn _ i j) s)      = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TDiv (AlexPn _ i j) s)     = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TMod (AlexPn _ i j) s)     = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TNotEq (AlexPn _ i j) s)   = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TEq (AlexPn _ i j) s)      = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TMoreEq (AlexPn _ i j) s)  = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TLessEq (AlexPn _ i j) s)  = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TReturn (AlexPn _ i j) s)  = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TAssign (AlexPn _ i j) s)  = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TPlus (AlexPn _ i j) s)    = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TMinus (AlexPn _ i j) s)   = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TStar (AlexPn _ i j) s)    = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TSlash (AlexPn _ i j) s)   = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TOpenP (AlexPn _ i j) s)   = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TCloseP (AlexPn _ i j) s)  = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TLess (AlexPn _ i j) s)    = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TMore (AlexPn _ i j) s)    = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TPercent (AlexPn _ i j) s) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TComma (AlexPn _ i j) s)   = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TSColon (AlexPn _ i j) s)  = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TBoolean (AlexPn _ i j) s) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TNumber (AlexPn _ i j) s)  = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TTrue (AlexPn _ i j) s)    = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TFalse (AlexPn _ i j) s)   = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TIdent (AlexPn _ i j) s)   = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TNum (AlexPn _ i j) s)     = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TString (AlexPn _ i j) s)  = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TUndef (AlexPn _ i j) s)   = "linea " ++ show i ++ ", columna " ++ show j


show_val :: Token -> String
show_val (TProgram p s) = s
show_val (TRead p s)    = s
show_val (TWriteLn p s) = s
show_val (TWrite p s)   = s
show_val (TEnd p s)     = s
show_val (TWith p s)    = s
show_val (TDo p s)      = s
show_val (TIf p s)      = s
show_val (TThen p s)    = s
show_val (TElse p s)    = s
show_val (TWhile p s)   = s
show_val (TFor p s)     = s
show_val (TFrom p s)    = s
show_val (TTo p s)      = s
show_val (TRepeat p s)  = s
show_val (TTimes p s)   = s
show_val (TFunc p s)    = s
show_val (TNot p s)     = s
show_val (TAnd p s)     = s
show_val (TOr p s)      = s
show_val (TDiv p s)     = s
show_val (TMod p s)     = s
show_val (TNotEq p s)   = s
show_val (TEq p s)      = s
show_val (TMoreEq p s)  = s
show_val (TLessEq p s)  = s
show_val (TReturn p s)  = s
show_val (TAssign p s)  = s
show_val (TPlus p s)    = s
show_val (TMinus p s)   = s
show_val (TStar p s)    = s
show_val (TSlash p s)   = s
show_val (TOpenP p s)   = s
show_val (TCloseP p s)  = s
show_val (TLess p s)    = s
show_val (TMore p s)    = s
show_val (TPercent p s) = s
show_val (TComma p s)   = s
show_val (TSColon p s)  = s
show_val (TBoolean p s) = s
show_val (TNumber p s)  = s
show_val (TTrue p s)    = s
show_val (TFalse p s)   = s
show_val (TIdent p s)   = s
show_val (TNum p s)     = show s
show_val (TString p s)  = s
show_val (TUndef p s)   = s


show_token:: Token -> String
show_token tok = show_pos tok ++ ": " ++ show_type tok ++ " '" ++ show_val tok ++ "'"

undef :: Token -> Bool
undef (TProgram p s) = False
undef (TRead p s)    = False
undef (TWriteLn p s) = False
undef (TWrite p s)   = False
undef (TEnd p s)     = False
undef (TWith p s)    = False
undef (TDo p s)      = False
undef (TIf p s)      = False
undef (TThen p s)    = False
undef (TElse p s)    = False
undef (TWhile p s)   = False
undef (TFor p s)     = False
undef (TFrom p s)    = False
undef (TTo p s)      = False
undef (TRepeat p s)  = False
undef (TTimes p s)   = False
undef (TFunc p s)    = False
undef (TNot p s)     = False
undef (TAnd p s)     = False
undef (TOr p s)      = False
undef (TDiv p s)     = False
undef (TMod p s)     = False
undef (TNotEq p s)   = False
undef (TEq p s)      = False
undef (TMoreEq p s)  = False
undef (TLessEq p s)  = False
undef (TReturn p s)  = False
undef (TAssign p s)  = False
undef (TPlus p s)    = False
undef (TMinus p s)   = False
undef (TStar p s)    = False
undef (TSlash p s)   = False
undef (TOpenP p s)   = False
undef (TCloseP p s)  = False
undef (TLess p s)    = False
undef (TMore p s)    = False
undef (TPercent p s) = False
undef (TComma p s)   = False
undef (TSColon p s)  = False
undef (TBoolean p s) = False
undef (TNumber p s)  = False
undef (TTrue p s)    = False
undef (TFalse p s)   = False
undef (TIdent p s)   = False
undef (TNum p s)     = False
undef (TString p s)  = False
undef (TUndef p s)   = True


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