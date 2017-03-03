-- Carlos Infante 13-10681
-- Rubmary Rojas 13-11264
-- Analizador sintáctico para el lenguaje "Retina".

{
module Parser where
import Lexer
import TokenInfo
import Tree
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
    id      { TIdent _ _ }
    num     { TNum _ _ }
    str     { TString _ _ }


%left or
%left and
%nonassoc '>=' '>' '<=' '<' '/=' '=='
%left '+' '-'
%left '*' '/' '%' div mod
%right NEG not

%%

-- inicio
S : Funs program Is end ';'         {Init $1 $3}

-- secuencia de instrucciones
Is : {- empty -}                    {[]}
    | Ins ';' Is                    {[$1] ++ $3}

-- expresiones
Exp : Exp '+' Exp                   {ESum   (Sum    $1 $3)}
    | Exp '-' Exp                   {EDif   (Dif    $1 $3)}
    | Exp '*' Exp                   {EMul   (Mul    $1 $3)}
    | Exp '/' Exp                   {EDiv   (Div    $1 $3)}
    | Exp '%' Exp                   {EMod   (Mod    $1 $3)}
    | Exp div Exp                   {EDivI  (DivI   $1 $3)}
    | Exp mod Exp                   {EModI  (ModI   $1 $3)}
    | '(' Exp ')'                   {$2}
    | '-' Exp %prec NEG             {ENeg $2}
    | num                           {EToken $1}
    | Exp or Exp                    {EOr    (Or     $1 $3)}
    | Exp and Exp                   {EAnd   (And    $1 $3)}
    | not Exp                       {ENot   $2}
    | Exp '>=' Exp                  {EGeq   (Geq    $1 $3)}
    | Exp '>' Exp                   {EGr    (Gr     $1 $3)}
    | Exp '<=' Exp                  {ELeq   (Leq    $1 $3)}
    | Exp '<' Exp                   {ELess  (Less   $1 $3)}
    | Exp '/=' Exp                  {ENeq   (Neq    $1 $3)}
    | Exp '==' Exp                  {EEqual (Equal  $1 $3)}
    | true                          {EToken $1}
    | false                         {EToken $1}
    | id                            {EToken $1}
    | FCall                         {EFCall $1}


-- funciones
Funs : {- empty -}                  {[]}
    | DefFunc ';' Funs              {[$1] ++ $3}

DefFunc : DFun                      {$1}
    | DFunR                         {$1}

    
Ret : return Exp                    {Ret $2}

DFun : func id '(' Pars ')' begin Is end  {DFun $2 $4 $7}

DFunR : func id '(' Pars ')' '->' Type begin Is end {DFunR $2 $4 $7 $9}

Pars : {- empty -}                  {[]}
    | Ps                            {$1}
Ps : Par                            {[$1]}
    | Par ',' Ps                    {[$1] ++ $3}

Par : Type id                       {Par $1 $2}

Type : number                       {$1}
    | boolean                       {$1}

-- llamada a funciones
FCall: id '(' ExpS ')'              {FCall $1 $3}
ExpS : {- empty -}                  {[]}
    | Es                            {$1}
Es  : Exp                           {[$1]}
    | Exp ',' Es                    {[$1] ++ $3}


-- declaraciones
Ds : {- empty -}                    {[]}
    | Dec ';' Ds                    {[$1] ++ $3}

Dec: Type id                        {Dec1 $1 [$2]}
    | Type id '=' Exp               {Dec2 $1 $2 $4}
    | Type id ',' Ids               {Dec1 $1 ([$2] ++ $4)}

Ids: id                             {[$1]}
    | id ',' Ids                    {[$1] ++ $3}


-- tipos de instrucciones

Ins : {- empty -}                   {IEmpty}
    | Block                         {IBlock   $1}
    | Read                          {IReadId  $1}
    | Write                         {IWrite   $1}
    | WriteL                        {IWriteL  $1}
    | Assig                         {IAssig   $1}
    | FCall                         {IFcall   $1}
    | Ret                           {IRet     $1}



Assig : id '=' Exp                  {Assig  $1 $3}
Read : read id                      {ReadId $2}
Write : write Prints                {Write  $2}
WriteL : writeln Prints             {WriteL $2}
Print : str                         {PToken $1}
    | Exp                           {PExp   $1}
Prints : Print                      {[$1]}
    | Print ',' Prints              {[$1] ++ $3}
    

-- Estructuras de control (bloques)
Block : Do                          {BDo      $1}
    | If                            {BIf      $1}
    | IfElse                        {BIfElse  $1}
    | While                         {BWhile   $1}
    | For                           {BFor     $1}
    | ForBy                         {BForby   $1}
    | Repeat                        {BRepeat  $1}


Do : with Ds do Is end                              {Do     $2 $4}
If : if Exp then Is end                             {If     $2 $4}
IfElse : if Exp then Is else Is end                 {IfElse $2 $4  $6}
While : while Exp do Is end                         {While  $2 $4}
For : for id from Exp to Exp do Is end              {For    $2 $4 $6 $8}
ForBy : for id from Exp to Exp by Exp do Is end     {ForBy  $2 $4 $6 $8 $10}
Repeat : repeat Exp times Is end                    {Repeat $2 $4}



{
parseError :: [Token] -> a
parseError [] = error $ "Final inesperado"
parseError t = error $ show_pos (head t) ++ ": token inesperado: " ++ show_val (head t)
}
