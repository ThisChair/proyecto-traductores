module Main(main) where

import Control.Monad.RWS
import Tree
import Data.Tree
import TokenInfo
import Lexer
import Data.Sequence as S
import Data.Map as M
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

-- Arbol inicial, solo para pruebas
initialTree :: Tree TypeNode
initialTree = (Node (IsToken (TFalse (AlexPn 0 0 0))) [Node For [], Node ForBy [], Node Is[]])


walkTree :: Tree TypeNode -> RetMonad ()
walkTree (Node Init (funs : program : is : end )) = do
  tell $ S.singleton "Funciones iniciales:"
  walkTree funs
  tell $ S.singleton "program"
  walkTree is
walkTree (Node Funs fs) = do
  mapM_ function fs
walkTree (Node Is is) = do
  mapM_ instruction is
walkTree (Node Exp ls) = do
  info <- calcExp (Node Exp ls)
  return ()
walkTree _ = do return ()



function :: Tree TypeNode -> RetMonad ()
function tree = do
  scope <- get                                    -- obtiene todo el estado del monad, lo guarda en scope
  let tableFunc   = sym scope                     -- obtiene la tabla de los simbolos de las funciones
  let id          = getId tree                    -- obtiene el identificador de la funcion
  let types       = getTypes tree                 -- obtiene los tipos de los parametros de la funcion
  let ids         = getIds tree                   -- obtiene una lista con los identificadores de los parametros de la funcion
  let typeF       = getTypeF tree                 -- obtiene el tipo de retorno de la funcion
  let is          = getIs                         -- obtiene el subarbol con las instrucciones de la funcion
  let repeatPar = True -- diff par
  let isInTable = True --M.member ID tableFunc
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




-- obtiene el identificador de la funcion
getId :: Tree TypeNode -> String
getId (Node DFun  ((Node (IsToken (TIdent _ id)) []): _ ) ) = id
getId (Node DFunR ((Node (IsToken (TIdent _ id)) []): _ ) ) = id



-- obtiene los identificadores de los parametros de la funcion
getIds :: Tree TypeNode -> [String]
getIds (Node DFun   ( _ : _ : (Node Pars pars) : _ )) = P.map getVariables pars
  where getVariables (Node Par ( _ : (Node (IsToken (TIdent _ id)) []) : _ )) = id
getIds (Node DFunR  ( _ : _ : (Node Pars pars) : _ )) = P.map getVariables pars
  where getVariables (Node Par ( _ : (Node (IsToken (TIdent _ id)) []) : _ )) = id

-- obtiene el tipo de retorno de la funcion, cuando la funcion no devuelve nada
-- el tipo de retorno es 'Void'
getTypeF :: Tree TypeNode -> Type
getTypeF (Node DFun _ ) = Void
getTypeF (Node DFunR ( _ : _ : _ : _ : _ : _ : _ : (Node (IsToken (TNumber   _ )) []) : [] )) = Number
getTypeF (Node DFunR ( _ : _ : _ : _ : _ : _ : _ : (Node (IsToken (TBoolean  _ )) []) : [] )) = Boolean


-- obtiene una lista con los tipos de los parametros de la funcion, importante 
-- para saber luego si una funcion fue llamada con parametros correctos
getTypes :: Tree TypeNode -> [Type]
getTypes (Node DFun   ( _ : _ : (Node Pars pars) : _ )) = P.map types pars
  where types (Node Par ( (Node (IsToken (TNumber   _ )) []) : _) ) = Number 
        types (Node Par ( (Node (IsToken (TBoolean  _ )) []) : _) ) = Boolean
getTypes (Node DFunR  ( _ : _ : (Node Pars pars) : _ )) = P.map types pars
  where types (Node Par ( (Node (IsToken (TNumber   _ )) []) : _) ) = Number 
        types (Node Par ( (Node (IsToken (TBoolean  _ )) []) : _) ) = Boolean


-- obtiene el subarbol con las instrucciones de la funcion 
getIs :: Tree TypeNode -> Tree TypeNode
getIs (Node DFun   ( _ : _ : _ : _  : is : _ )) = is
getIs (Node DFunR  ( _ : _ : _ : _  : is : _ )) = is


instruction :: Tree TypeNode -> RetMonad ()
instruction tree = do return ()




calcExp :: Tree TypeNode -> RetMonad (Int)
calcExp (Node Exp _ ) = do
  return 5


main = do 
  let runWT = runRWS (walkTree initialTree) "" initialState
  
  putStrLn $ show runWT
















