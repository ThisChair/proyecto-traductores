{
module Parser where
import Lexer
}

%name parseRet
%tokentype { Token }
%error { parseError }

%token

    program { TProgram _ }
    read    { TRead _ }
    writeln { TWriteLn _ }
    write   { TWrite _ }
    end     { TEnd _ }
    with    { TWith _ }
    do      { TDo _ }
    if      { TIf _ }
    then    { TThen _ }
    else    { TElse _ }
    while   { TWhile _ }
    for     { TFor _ }
    from    { TFrom _ }
    to      { TTo _ }
    by      { TBy _ }
    repeat  { TRepeat _ }
    times   { TTimes _ }
    func    { TFunc _ }
    begin   { TBegin _ }
    not     { TNot _ }
    and     { TAnd _ }
    or      { TOr _ }
    div     { TDiv _ }
    mod     { TMod _ }
    '/='    { TNotEq _ }
    '=='    { TEq _ }
    '>='    { TMoreEq _ }
    '<='    { TLessEq _ }
    '->'    { TArrow _ }
    return  { TReturn _ }
    '='     { TAssign _ }
    '+'     { TPlus _ }
    '-'     { TMinus _ }
    '*'     { TStar _ }
    '/'     { TSlash _ }
    '('     { TOpenP _ }
    ')'     { TCloseP _ }
    '<'     { TLess _ }
    '>'     { TMore _ }
    '%'     { TPercent _ }
    ','     { TComma _ }
    ';'     { TSColon _ }
    boolean { TBoolean _ }
    number  { TNumber _ }
    true    { TTrue _ }
    false   { TFalse _ }
    id      { TIdent _ $$ }
    num     { TNumber _ $$ }
    str     { TString _ $$ }

%left or
%left and
%nonassoc '>=' '>' '<=' '<' '/=' '=='
%left '+' '-'
%left '*' '/' '%' div mod
%left NEG not

%%

S : Funs program Is end ';' {[]}

Funs : {- empty -}     {[]}
    | Funs DefFunc ';' {[]}

Is : {- empty -} {[]}
    | Is Ins ';' {[]}

Exp : BoolE       {[]}
    | AritE       {[]}
    
AritE : AritE '+' AritE   {[]}
    | AritE '-' AritE     {[]}
    | AritE '*' AritE     {[]}
    | AritE '/' AritE     {[]}
    | AritE '%' AritE     {[]}
    | AritE div AritE     {[]}
    | AritE mod AritE     {[]}
    | '(' AritE ')'       {[]}
    | '-' AritE %prec NEG {[]}
    | num                 {[]}
    | id                  {[]}
    
BoolE : BoolE or BoolE {[]}
    | BoolE and BoolE  {[]}
    | '(' BoolE ')'    {[]}
    | not BoolE        {[]}
    | AComp            {[]}
    | BComp            {[]}
    | true             {[]}
    | false            {[]}
    | id               {[]}

AComp : AritE '>=' AritE {[]}
    | AritE '>' AritE    {[]}
    | AritE '<=' AritE   {[]}
    | AritE '<' AritE    {[]}
    | AritE '/=' AritE   {[]}
    | AritE '==' AritE   {[]}

BComp : BoolE '/=' BoolE {[]}
    | BoolE '==' BoolE   {[]}

Type : number {[]}
    | boolean {[]}

Dec: Type id '=' Exp {[]}
    | Type Ids id  {[]}

Ids : {- empty -} {[]}
    | Ids id ','  {[]}

Assig : id '=' Exp {[]}

Read : read id {[]}

Write : write Prints Print {[]}

WriteL : writeln Prints Print {[]}

Print : str {[]}
    | Exp {[]}

Prints : {- empty -} {[]}
    | Prints Print ',' {[]}

DefFunc : DFun {[]}
    | DFunR {[]}

DFun : func id '(' Pars ')' begin Is end {[]}

DFunR : func id '(' Pars ')' '->' Type begin Is end {[]}

Pars : {- empty -} {[]}
    | Ps Par       {[]}

Ps : {-empty-} {[]}
    | Ps Par ','  {[]}

Par : Type id {[]}

Block : Do   {[]}
    | If     {[]}
    | IfElse {[]}
    | While  {[]}
    | For    {[]}
    | ForBy  {[]}
    | Repeat {[]}

Do : with Dec do Is end {[]}

If : if BoolE then Is end {[]}

IfElse : if BoolE then Is else Is end {[]}

While : while BoolE do Is end {[]}

For : for id from AritE to AritE do Is end {[]}

ForBy : for id from AritE to AritE by AritE do Is end {[]}

Repeat : repeat AritE times Is end {[]}

Ins : Block  {[]}
    | Read   {[]}
    | Write  {[]}
    | WriteL {[]}
    | Assig  {[]}

{

parseError :: [Token] -> a
parseError _ = error "Parse error"

}