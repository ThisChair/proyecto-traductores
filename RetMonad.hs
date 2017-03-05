-- Recordar que falta el return
-- modificar estructura scope :(

module RetMonad where
import Control.Monad.RWS
import Tree
import TokenInfo
import Lexer
import Parser
import Data.Sequence as S
import Data.Map as M
import Data.Set as Set
import Prelude as P
import Data.Maybe

type SymScope = Map String Variable             -- Tabla de simbolo de un alcance en especifico
type SymTable = [SymScope]                      -- Tabla de simbolos, realmente es una lista de tablas de simbolos
type SymFunc  = Map String Function             -- Tabla de funciones, eso si es una sola tabla :)


-- Posibles tipos de retorno para las funciones, preguntar si una funcion puede
-- devolver un string 
data Type = Boolean | Number | Void
            deriving (Show, Eq)

-- Tipo para las variables guardadas en la tabla de simbolos
-- preguntar si una variable puede contener un string
data Variable = Variable { t :: Type
                         , num  :: Double
                         , bool :: Bool }
                         deriving (Show)


-- Tipo de datos para almacenar las funciones
data Function = Function { ret :: Type              -- tipo de valor de retorno de la funcion
                         , parameters :: [Type] }   -- tipo de los parametros de la fucion
                         deriving (Show)


-- tipo de dato del monad 
data Scope = Scope  { sym     :: SymTable           -- tabla de simbolos
                    , func    :: SymFunc            -- tabla de las funciones
                    , height  :: Int                -- altura del arbol
                    , count   :: Int }              -- contador que enumera los alcances
                    deriving (Show)
                
type RetMonad = RWS String (S.Seq(String)) Scope

-- Estado inicial del Monad
initialState =  Scope
                      []
                      M.empty
                      0
                      0



-----------------------------  FUNCIONES GLOBALES PARA TRABAJAR EL MONAD ---------------------------------

-- Funcion para modificar la tabla de simbolos
-- hacer modify (modifyTable f) donde f es una funcion
-- que toma la tabla de simbolos actual y devuelve otra tabla de simbolos
modifyTable :: (SymTable -> SymTable) -> Scope -> Scope
modifyTable f (Scope symTable x y z)    = Scope (f symTable) x y z


-- Usar cuando se tenga un nuevo alcance, agrega una nueva tabla de simbolos
addTable :: SymTable -> SymTable
addTable symTable = (M.empty:symTable)

-- Moficar ultimo alcance: recibe una funcion que modifica el ultimo alcance
-- hacer modify (modifyTable $ modifyScope f)
modifyScope :: (SymScope -> SymScope) -> SymTable -> SymTable
modifyScope f (s:xs) = (f s : xs)

-- Elimina el ultimo alcance
eraseLastScope :: SymTable -> SymTable
eraseLastScope (x:xs) = xs

-- Verificar si un identificador esta en la tabla de simbolos
-- si encuentra el identificador retorna su informacion
findSym :: SymTable -> String -> Maybe Variable
findSym []      id  = Nothing
findSym (ss:xs) id  = if (isNothing var) then (findSym xs id) else var 
  where var = M.lookup id ss


-- Modificar el valor de una variable en la tabla de simbolos
modifySym :: String -> Variable -> SymTable -> SymTable
modifySym id var (ss:xs) = if (isNothing var') then (ss : (modifySym id var xs)) else ((M.insert id var ss) : xs)
    where var' = M.lookup id ss

-- agregar un nuevo simbolo en el ultimo alcance de la tabla de simbolos
insertSym :: String -> Variable -> Scope -> Scope
insertSym id var = modifyTable (modifyScope (M.insert id var))

-- Verifica si un identificador esta en la tabla de funciones
-- si encuentra el identificador retorna su informacion
findFunc :: SymFunc -> String -> Maybe Function
findFunc symFun id = M.lookup id symFun 



