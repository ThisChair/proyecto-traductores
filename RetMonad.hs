module Main(main) where

import Control.Monad.RWS
import Tree
import TokenInfo
import Lexer
import Data.Sequence as S
import Data.Map as M
import Data.Set as Set
import Prelude as P

type SymTable = [Map String Variable]         -- Tabla de simbolos, realmente es una lista de tablas de simbolos
type SymFunc  = Map String Function           -- Tabla de funciones, eso si es una sola tabla :)


-- Posibles tipos de retorno para las funciones, preguntar si una funcion puede
-- devolver un string 
data Type = Boolean | Number | Void
            deriving (Show)

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



start :: Init -> RetMonad ()
start (Init funs is) = do
  tell $ S.singleton "Funciones iniciales:"
  mapM_ function funs
  tell $ S.singleton "program"
--  mapM_ instruction is



function :: DefFunc -> RetMonad ()
function (DFun identifier pars is) = do
  scope <- get                                                        -- obtiene todo el estado del monad, lo guarda en scope
  let tableFunc   = sym scope                                         -- obtiene la tabla de los simbolos de las funciones
  let typeF       = Void                                              -- tipo de retorno de la funcion
  let repeatPar   = (Set.size $ Set.fromList ids) != P.length ids     -- verifica si hay parametros con el mismo identificador
  let isInTable   = True --M.member ID tableFunc
  case isInTable of
    True -> do return ()-- errorRepeatFunc id
    False -> do return ()
  case repeatPar of
    True -> do return () -- errorRepeatPar par
    False -> do return ()
--  modify(insert id (Function t types))
--  modify(addSyms par)
--  walkTree is
--  modify (eraseLastScope)
  where (TIdent _ id) = identifier                                -- obtiene identificador de la funcion
        getType (Par (TBoolean _) (TIdent _ id))  = Boolean       -- funcion para obtener el tipo de un parametro
        getType (Par (TNumber _ ) (TIdent _ id))  = Number        -- funcion para obtener el tipo de un parametro
        getId   (Par _ (TIdent _ id))             = id            -- funcion para obtener el tipo de un parametro
        types = P.map getType pars                                -- obtener los tipos de los parametros
        ids   = P.map getId   pars                                -- obtener los identificadores de los parametros 









main :: IO ()
main = putStrLn "Ok"












