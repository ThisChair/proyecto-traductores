{
module Parser where
import Lexer
import TokenInfo
import Tree
import Data.Tree
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
%left NEG not

%%

S : Funs program Is end ';'         {Node Init [Node (IsToken $2) $3]}

Funs : {- empty -}                  {[]}
    | DefFunc ';' Funs              {[$1] ++ $3}

Is : {- empty -}                    {[]}
    | Ins ';' Is                    {[$1] ++ $3}
    
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

FCall: id '(' Exp ')'               {[]}    -- Lista de expresiones

Type : number                       {Node (IsToken $1) []}
    | boolean                       {Node (IsToken $1) []}


Ds : {- empty -}                    {[]}
    | Dec ';' Ds                    {[$1] ++ $3}

Dec: Type id                        {Node Dec [$1, Node (IsToken $2) []]}
    | Type id '=' Exp               {Node Dec [$1, Node (IsToken $2) [], Node (IsToken $3) []]} -- Falta agregar el hijo de expresiones
    | Type id ',' Ids               {Node Dec ([$1, Node (IsToken $2) []] ++ $4)}

Ids: id                             {[Node (IsToken $1) []]}
    | id ',' Ids                    {[Node (IsToken $1) []] ++ $3}


Assig : id '=' Exp                  {Node Assig [Node (IsToken $1) [], Node (IsToken $2) []]}

Read : read id                      {Node Read [Node (IsToken $1) [], Node (IsToken $2) []]}

--Write : write Print Prints          {Node Write ([Node (IsToken $1) [], Node (IsToken $2) []] ++ $3)}

--WriteL : writeln Print Prints       {Node (IsToken $1) ([Node (IsToken $2) []] ++ $3)}

Print : str                         {Node (IsToken $1) []}
    | Exp                           {Node Exp []}

Prints : {- empty -}                {[]}
    | Print ',' Prints              {[$1] ++ $3}

DefFunc : DFun                      {$1}
    | DFunR                         {$1}
    


Block : Do                          {$1}
    | If                            {$1}
--    | IfElse                        {}
--    | While                         {}
--    | For                           {}
--    | ForBy                         {}
--    | Repeat                        {}

Ins : Block  {$1}
--    | Read   {}
--    | Write  {}
--    | WriteL {}
    | Assig  {$1}
--    | FCall  {}

DFun : func id '(' Pars ')' begin Is end {Node (IsToken $1) [Node (IsToken $2) [], Node Pars $4, Node (IsToken $6) $7]}

DFunR : func id '(' Pars ')' '->' Type begin FBody end {Node (IsToken $1) [Node (IsToken $2) [], Node Pars $4]}

FBody : {- empty -}                                 {[]}
    | Ret ';' FBody                                 {[$1] ++ $3}
    | Ins ';' FBody                                 {[$1] ++ $3}
    
Ret : return Exp                                    {Node (IsToken $1) []}

Pars : {- empty -}                                  {[]}
    | Ps                                            {$1}

Ps : Par                                            {[$1]}
    | Par ',' Ps                                    {[$1] ++ $3}


Par : Type id                                       {Node Par [$1, Node (IsToken $2) []]}

Do : with Ds do Is end                              {Node (IsToken $1) [Node Ds $2, Node Is $4]}

If : if Exp then Is end                             {Node If [Node (IsToken $1) [], Node Exp [], Node (IsToken $3) $4]}

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
