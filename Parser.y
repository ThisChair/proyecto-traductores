{
module Parser where
import Lexer
import TokenInfo
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
    num     { TNum _ $$ }
    str     { TString _ $$ }


%left or
%left and
%nonassoc '>=' '>' '<=' '<' '/=' '=='
%left '+' '-'
%left '*' '/' '%' div mod
%left NEG not

%%

S : Funs program Is end ';'         {[0]}

Funs : {- empty -}                  {[0]}
    | Funs DefFunc ';'              {[0]}

Is : {- empty -}                    {[0]}
    | Is Ins ';'                    {[0]}
    
Exp : Exp '+' Exp                   {[0]}
    | Exp '-' Exp                   {[0]}
    | Exp '*' Exp                   {[0]}
    | Exp '/' Exp                   {[0]}
    | Exp '%' Exp                   {[0]}
    | Exp div Exp                   {[0]}
    | Exp mod Exp                   {[0]}
    | '(' Exp ')'                   {[0]}
    | '-' Exp %prec NEG             {[0]}
    | num                           {[0]}
    | Exp or Exp                    {[0]}
    | Exp and Exp                   {[0]}
    | not Exp                       {[0]}
    | Exp '>=' Exp                  {[0]}
    | Exp '>' Exp                   {[0]}
    | Exp '<=' Exp                  {[0]}
    | Exp '<' Exp                   {[0]}
    | Exp '/=' Exp                  {[0]}
    | Exp '==' Exp                  {[0]}
    | true                          {[0]}
    | false                         {[0]}
    | id                            {[0]}
    | FCall                         {[0]}

FCall: id '(' Exp ')'               {[0]}    

Type : number                       {[0]}
    | boolean                       {[0]}
--   | Expan {[]}

Ds : {- empty -}                    {[0]}
    | Ds Dec ';'                    {[0]}

Dec: Type id                        {[0]}
    | Type id '=' Exp               {[0]}
    | Type id ',' Ids               {[0]}

Ids: id                             {[0]}
    | id ',' Ids                    {[0]}


Assig : id '=' Exp                  {[0]}

Read : read id                      {[0]}

Write : write Prints Print          {[0]}

WriteL : writeln Prints Print       {[0]}

Print : str                         {[]}
    | Exp                           {[]}

Prints : {- empty -}                {[]}
    | Prints Print ','              {[]}

DefFunc : DFun                      {[]}
    | DFunR                         {[]}
    
Ps : {-empty-}                      {[]}
    | Ps Par ','                    {[]}

Par : Type id                       {[]}

Block : Do                          {[]}
    | If                            {[]}
    | IfElse                        {[]}
    | While                         {[]}
    | For                           {[]}
    | ForBy                         {[]}
    | Repeat                        {[]}

Ins : Block  {[]}
    | Read   {[]}
    | Write  {[]}
    | WriteL {[]}
    | Assig  {[]}
    | FCall  {[]}

DFun : func id '(' Pars ')' begin FBody end {[]}

DFunR : func id '(' Pars ')' '->' Type begin FBody end {[]}

FBody : {- empty -} {[]}
    | FBody Ret ';' {[]}
    | FBody Ins ';' {[]}
    
Ret : return Exp ';' {[]}

Pars : {- empty -}                                  {[]}
    | Ps Par                                        {[]}

Do : with Ds do Is end                              {[]}

If : if Exp then Is end                             {[]}

IfElse : if Exp then Is else Is end                 {[]}

While : while Exp do Is end                         {[]}

For : for id from Exp to Exp do Is end              {[]}

ForBy : for id from Exp to Exp by Exp do Is end     {[]}

Repeat : repeat Exp times Is end                    {[]}



{

parseError :: [Token] -> a
parseError [] = error $ "Final inesperado"
parseError t = error $ show_pos (head t) ++ ": token inesperado: " ++ show_val (head t)

}
