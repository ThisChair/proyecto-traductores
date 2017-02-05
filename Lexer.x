{
module Lexer where

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
    
}
