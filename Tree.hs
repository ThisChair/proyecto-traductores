-- Carlos Infante 13-10681
-- Rubmary Rojas 13-11264
-- Tipo de Dato Tree TypeNodo, para manejar el arbol generado por
-- el analizador lexicografico

module Tree where
import Lexer
import System.IO
import System.Environment
import TokenInfo
import Prelude as P



data Init = Init [DefFunc] [Ins] deriving (Show)


data Exp      = ESum    Sum       |
                EDif    Dif       |
                EMul    Mul       |
                EDiv    Div       |
                EMod    Mod       |
                EDivI   DivI      |
                EModI   ModI      |
                EOr     Or        |
                EAnd    And       |
                EGeq    Geq       |
                EGr     Gr        |
                ELeq    Leq       |
                ELess   Less      |
                ENeq    Neq       |
                EEqual  Equal     |
                ENeg    Exp       |
                ENot    Exp       |
                EFCall  FCall     |
                EToken  Token
                deriving (Show)

data Sum    = Sum   Exp Exp   deriving (Show)
data Dif    = Dif   Exp Exp   deriving (Show)
data Mul    = Mul   Exp Exp   deriving (Show)
data Div    = Div   Exp Exp   deriving (Show)
data Mod    = Mod   Exp Exp   deriving (Show)
data DivI   = DivI  Exp Exp   deriving (Show)
data ModI   = ModI  Exp Exp   deriving (Show)
data Or     = Or    Exp Exp   deriving (Show)
data And    = And   Exp Exp   deriving (Show)
data Geq    = Geq   Exp Exp   deriving (Show)
data Gr     = Gr    Exp Exp   deriving (Show)
data Leq    = Leq   Exp Exp   deriving (Show)
data Less   = Less  Exp Exp   deriving (Show)
data Neq    = Neq   Exp Exp   deriving (Show)
data Equal  = Equal Exp Exp   deriving (Show)

data DefFunc  = DFun  Token [Par] [Ins] |         -- Token: identificador de la función
                DFunR Token [Par] Token [Ins]      -- Token 1: identificador, Token 2: valor de retorno
                deriving (Show)
data FCall    = FCall Token [Exp]                 -- Token 1: identificador de la función
                deriving (Show)
data Dec      = Dec1 Token [Token] |              -- Declaración sin asignación, Token: tipo, [Token]: identificadores
                Dec2 Token Token Exp              -- Declaración con asignación, Token1: tipo, Token2: identificador 
                deriving (Show)
data Par      = Par   Token Token                 -- Token1: Tipo, Token2: identificador
                deriving (Show)
data Ret      = Ret Exp
                deriving (Show)
data Ins      = IBlock Block    |
                IReadId ReadId  |
                IWrite Write    |
                IWriteL WriteL  |
                IAssig Assig    |
                IFcall FCall    |
                IRet Ret        |
                IEmpty 
                deriving (Show)
                
data Assig    = Assig Token Exp  deriving (Show)          -- Token: identificador
data ReadId   = ReadId  Token    deriving (Show)          -- Token: identificador
data Write    = Write  [Print]   deriving (Show)
data WriteL   = WriteL [Print]   deriving (Show)
data Print    = PToken Token |                            -- Token: string para imprimir
                PExp Exp         deriving (Show)

data Block    = BDo Do          |
                BIf If          |
                BIfElse IfElse  |
                BWhile While    |
                BFor For        |
                BForby ForBy    |
                BRepeat Repeat deriving (Show) 

data Do       = Do [Dec] [Ins]                deriving (Show)
data If       = If Exp [Ins]                  deriving (Show)
data IfElse   = IfElse Exp [Ins] [Ins]        deriving (Show)
data While    = While Exp [Ins]               deriving (Show)
data For      = For   Token Exp Exp [Ins]     deriving (Show)         -- Token: identificador del contador, Exp1: inicio, Exp2: fin
data ForBy    = ForBy Token Exp Exp Exp [Ins] deriving (Show)         -- Token: identificador del contador, Exp1: inicio, Exp2: fin, Exp3: salto
data Repeat   = Repeat Exp [Ins]              deriving (Show)         -- Exp:   cantidad de repeticiones




              

 
 


