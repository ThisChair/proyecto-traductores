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
%right NEG not

%%

-- inicio
S : Funs program Is end ';'         {Node Init [Node Funs $1, Node (IsToken $2) [], Node Is $3, Node (IsToken $4) []]}

-- secuencia de instrucciones
Is : {- empty -}                    {[]}
    | Ins ';' Is                    {[$1] ++ $3}

-- expresiones
Exp : Exp '+' Exp                   {Node Exp [$1, Node (IsToken $2)[], $3]}
    | Exp '-' Exp                   {Node Exp [$1, Node (IsToken $2)[], $3]}
    | Exp '*' Exp                   {Node Exp [$1, Node (IsToken $2)[], $3]}
    | Exp '/' Exp                   {Node Exp [$1, Node (IsToken $2)[], $3]}
    | Exp '%' Exp                   {Node Exp [$1, Node (IsToken $2)[], $3]}
    | Exp div Exp                   {Node Exp [$1, Node (IsToken $2)[], $3]}
    | Exp mod Exp                   {Node Exp [$1, Node (IsToken $2)[], $3]}
    | '(' Exp ')'                   {Node Exp [Node (IsToken $1)[], $2, Node (IsToken $3)[]]}
--    | '-' Exp %prec NEG             {Node Exp [(IsToken $1)[], $2]}
    | num                           {Node (IsToken $1) []}
    | Exp or Exp                    {Node Exp [$1, Node (IsToken $2)[], $3]}
    | Exp and Exp                   {Node Exp [$1, Node (IsToken $2)[], $3]}
--    | not Exp                       {Node Exp [(IsToken $1)[], $2]}
    | Exp '>=' Exp                  {Node Exp [$1, Node (IsToken $2)[], $3]}
    | Exp '>' Exp                   {Node Exp [$1, Node (IsToken $2)[], $3]}
    | Exp '<=' Exp                  {Node Exp [$1, Node (IsToken $2)[], $3]}
    | Exp '<' Exp                   {Node Exp [$1, Node (IsToken $2)[], $3]}
    | Exp '/=' Exp                  {Node Exp [$1, Node (IsToken $2)[], $3]}
    | Exp '==' Exp                  {Node Exp [$1, Node (IsToken $2)[], $3]}
    | true                          {Node (IsToken $1) []}
    | false                         {Node (IsToken $1) []}
    | id                            {Node (IsToken $1) []}
    | FCall                         {$1}


-- funciones
Funs : {- empty -}                  {[]}
    | DefFunc ';' Funs              {[$1] ++ $3}

DefFunc : DFun                      {$1}
    | DFunR                         {$1}

FBody : {- empty -}                 {[]}
    | Ret ';' FBody                 {[$1] ++ $3}
    | Ins ';' FBody                 {[$1] ++ $3}
    
Ret : return Exp                    {Node (IsToken $1) []}

DFun : func id '(' Pars ')' begin Is end  { Node DFun [Node (IsToken $1)[], Node (IsToken $2) [], Node Pars $4, Node (IsToken $6) [], Node Is $7, Node (IsToken $8) []]}

DFunR : func id '(' Pars ')' '->' Type begin FBody end {Node DFunR [Node (IsToken $1) [], Node (IsToken $2) [], Node Pars $4, Node  (IsToken $6) [], $7, Node (IsToken $8) [], Node FBody $9, Node (IsToken $10) []]}

Pars : {- empty -}                  {[]}
    | Ps                            {$1}
Ps : Par                            {[$1]}
    | Par ',' Ps                    {[$1] ++ $3}

Par : Type id                       {Node Par [$1, Node (IsToken $2) []]}

Type : number                       {Node (IsToken $1) []}
    | boolean                       {Node (IsToken $1) []}

-- llamada a funciones
FCall: id '(' ExpS ')'              {Node FCall [Node (IsToken $1) [], Node ExpS $3]}
ExpS : {- empty -}                  {[]}
    | Es                            {$1}
Es  : Exp                           {[Node Exp []]}
    | Exp ',' Es                    {[Node Exp []] ++ $3}


-- declaraciones
Ds : {- empty -}                    {[]}
    | Dec ';' Ds                    {[$1] ++ $3}

Dec: Type id                        {Node Dec [$1, Node (IsToken $2) []]}
    | Type id '=' Exp               {Node Dec [$1, Node (IsToken $2) [], Node (IsToken $3) []]} -- Falta agregar el hijo de expresiones
    | Type id ',' Ids               {Node Dec ([$1, Node (IsToken $2) []] ++ $4)}

Ids: id                             {[Node (IsToken $1) []]}
    | id ',' Ids                    {[Node (IsToken $1) []] ++ $3}


-- tipos de instrucciones

Ins : Block                         {$1}
    | Read                          {$1}
    | Write                         {$1}
    | WriteL                        {$1}
    | Assig                         {$1}
    | FCall                         {$1}


Assig : id '=' Exp                  {Node Assig [Node (IsToken $1) [], Node (IsToken $2) []]}
Read : read id                      {Node Read [Node (IsToken $1) [], Node (IsToken $2) []]}
Write : write Prints                {Node Write [Node (IsToken $1) [], Node Prints $2]}
WriteL : writeln Prints             {Node WriteL [Node (IsToken $1) [], Node Prints $2]}
Print : str                         {Node (IsToken $1) []}
    | Exp                           {Node Exp []}
Prints : Print                      {[]}
    | Print ',' Prints              {[$1] ++ $3}
    

-- bloques
Block : Do                          {$1}
    | If                            {$1}
    | IfElse                        {$1}
    | While                         {$1}
    | For                           {$1}
    | ForBy                         {$1}
    | Repeat                        {$1}


Do : with Ds do Is end                              {Node Do [Node (IsToken $1) [], Node Ds $2, Node (IsToken $3) [], Node Is $4, Node (IsToken $5) []]}
If : if Exp then Is end                             {Node If [Node (IsToken $1) [], Node Exp [], Node (IsToken $3) [], Node Is $4, Node (IsToken $5) []]}
IfElse : if Exp then Is else Is end                 {Node IfElse [Node (IsToken $1) [], Node Exp [], Node (IsToken $3) [], Node Is $4, Node (IsToken $5) [], Node Is $6, Node (IsToken $7) []]}
While : while Exp do Is end                         {Node While [Node (IsToken $1) [], Node Exp [], Node (IsToken $3) [], Node Is $4, Node (IsToken $5) []]}
For : for id from Exp to Exp do Is end              {Node For [Node (IsToken $1) [], Node (IsToken $2) [], Node (IsToken $3) [], Node Exp[], Node (IsToken $5) [], Node Exp [], Node (IsToken $7) [], Node Is $8, Node (IsToken $9) []]}
ForBy : for id from Exp to Exp by Exp do Is end     {Node ForBy [Node (IsToken $1) [], Node (IsToken $2) [], Node (IsToken $3) [], Node Exp[], Node (IsToken $5) [], Node Exp [], Node (IsToken $7) [], Node Exp [], Node (IsToken $9) [], Node Is $10, Node (IsToken $11) []]}
Repeat : repeat Exp times Is end                    {Node Repeat [Node (IsToken $1) [], Node Exp [], Node (IsToken $3)[], Node Is $4, Node (IsToken $5) []]}



{
parseError :: [Token] -> a
parseError [] = error $ "Final inesperado"
parseError t = error $ show_pos (head t) ++ ": token inesperado: " ++ show_val (head t)
}
