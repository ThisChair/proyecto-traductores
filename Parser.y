{
module Parser where
import Lexer
}

%name parseRet
%tokentype { Token }
%error { parseError }

%token

    program {TProgram}
    read    {TRead}
    writeln {TWriteLn}
    write   {TWrite}
    end     {TEnd}
    with    {TWith}
    do      {TDo}
    if      {TIf}
    then    {TThen}
    else    {TElse}
    while   {TWhile}
    for     {TFor}
    from    {TFrom}
    to      {TTo}
    repeat  {TRepeat}
    times   {TTimes}
    func    {TFunc}
    not     {TNot}
    and     {TAnd}
    or      {TOr}
    div     {TDiv}
    mod     {TMod}
    '/='    {TNotEq}
    '=='    {TEq}
    '>='    {TMoreEq}
    '<='    {TLessEq}
    '->'    {TArrow}
    return  {TReturn}
    '='     {TAssign}
    '+'     {TPlus}
    '-'     {TMinus}
    '*'     {TStar}
    '/'     {TSlash}
    '('     {TOpenP}
    ')'     {TCloseP}
    '<'     {TLess}
    '>'     {TMore}
    '%'     {TPercent}
    ','     {TComma}
    ';'     {TSColon}
    boolean {TBoolean}
    number  {TNumber}
    true    {TTrue}
    false   {TFalse}
    id      {TIdent}
    num     {TNumber}
    str     {TString}

%left or
%left and
%nonassoc '>=' '>' '<=' '<' '/=' '=='
%left '+' '-'
%left '*' '/' '%' div mod
%left NEG not

%%

Funs : {- empty -}
    | Funs DefFunc ';'

Is : {- empty -}
    | Is Ins ';'

S : Funs program Is end ';'

Exp : BoolE
    | AritE
    | '(' Exp ')'
    
AritE : AritE '+' AritE
    | AritE '-' AritE
    | AritE '*' AritE
    | AritE '/' AritE
    | AritE '%' AritE
    | AritE div AritE
    | AritE mod AritE
    | '-' AritE %prec NEG
    | num
    | id
    
BoolE : BoolE or BoolE
    | BoolE and BoolE
    | not BoolE
    | AComp
    | BComp
    | true
    | false
    | id

AComp : AritE '>=' AritE
    | AritE '>' AritE
    | AritE '<=' AritE
    | AritE '<' AritE
    | AritE '/=' AritE
    | AritE '==' AritE

BComp : BoolE '/=' BoolE
    | BoolE '==' BoolE

Type : number
    | bool

Dec: Type id = Exp
    | Type Ids id

Ids : {- empty -}
    | Ids id ','

Assig : id = Exp

Read : read id

Write : write Prints Print

WriteL : writeln Prints Print

Print : str
    | Exp

Prints : {- empty -}
    | Prints Print ','

DefFunc : DFun
    | DFunR

DFun : func id '(' Pars ')' begin Is end

DFunR : func id '(' Pars ')' '->' Type begin Is end

Pars : {- empty -}
    | Ps Par

Ps : {-empty-}
    | Ps Par,

Par : Type id

Block : Do
    | If
    | IfElse
    | While
    | For
    | ForBy
    | Repeat

Do : with Dec do Is end

If : if BoolE then Is end

IfElse : if BoolE then Is end

While : while BoolE do Is end

For : for id from AritE to AritE do Is end

ForBy : for id from AritE to AritE by AritE do Is end

Repeat : repeat AritE times Is end

Ins : Block
    | Read
    | Write
    | WriteL
    | Assig

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

}