module TokenInfo where
import Lexer

show_pos :: Token -> String
show_pos (TProgram  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TRead  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TWriteLn  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TWrite  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TBegin  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TEnd  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TWith  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TBy  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TDo  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TIf  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TThen  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TElse  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TWhile  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TFor  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TFrom  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TTo  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TRepeat  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TTimes  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TFunc  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TNot  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TAnd  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TOr  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TDiv  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TReturn  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TMod  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TNotEq  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TEq  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TMoreEq  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TLessEq  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TAssign  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TPlus   (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TMinus  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TStar  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TSlash  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TOpenP  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TCloseP  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TLess  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TMore  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TPercent  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TComma  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TSColon  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TArrow  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TBoolean  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TNumber  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TTrue  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TFalse  (AlexPn _ i j)) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TIdent  (AlexPn _ i j) s) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TNum  (AlexPn _ i j) s) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TString  (AlexPn _ i j) s) = "linea " ++ show i ++ ", columna " ++ show j
show_pos (TUndef (AlexPn _ i j) s)   = "linea " ++ show i ++ ", columna " ++ show j



show_val :: Token -> String
show_val (TProgram _) = "program"
show_val (TRead _) = "read"
show_val (TWriteLn _) = "writeln"
show_val (TWrite _) = "write"
show_val (TBegin _) = "begin"
show_val (TEnd _) = "end"
show_val (TWith _) = "with"
show_val (TBy _) = "by"
show_val (TDo _) = "do"
show_val (TIf _) = "if"
show_val (TThen _) = "then"
show_val (TElse _) = "else"
show_val (TWhile _) = "while"
show_val (TFor _) = "for"
show_val (TFrom _) = "from"
show_val (TTo _) = "to"
show_val (TRepeat _) = "repeat"
show_val (TTimes _) = "times"
show_val (TFunc _) = "func"
show_val (TNot _) = "not"
show_val (TAnd _) = "and"
show_val (TOr _) = "or"
show_val (TDiv _) = "div"
show_val (TReturn _) = "return"
show_val (TMod _) = "mod"
show_val (TNotEq _) = "/="
show_val (TEq _) = "=="
show_val (TMoreEq _) = ">="
show_val (TLessEq _) = "<="
show_val (TAssign _) = "="
show_val (TPlus _) = "+"
show_val (TMinus _) = "-"
show_val (TStar _) = "*"
show_val (TSlash _) = "/"
show_val (TOpenP _) = "("
show_val (TCloseP _) = ")"
show_val (TLess _) = "<"
show_val (TMore _) = ">"
show_val (TPercent _) = "%"
show_val (TComma _) = ","
show_val (TSColon _) = ";"
show_val (TArrow _) = "->"
show_val (TBoolean _) = "boolean"
show_val (TNumber _) = "number"
show_val (TTrue _) = "true"
show_val (TFalse _) = "false"
show_val (TIdent _ s) = "identificador: " ++ s
show_val (TNum _ s) = "literal numérico: " ++ show s
show_val (TString _ s) = "literal string: " ++ s
show_val (TUndef p s)   = s



show_token:: Token -> String
show_token tok = show_pos tok ++ ": caracter inesperado" ++ " '" ++ show_val tok ++ "'"


undef :: Token -> Bool
undef (TUndef p s) = True
undef _ = False