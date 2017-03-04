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


-- agregar un nuevo simbolo en el ultimo alcance de la tabla de simbolos
insertSym :: String -> Type -> Double -> Bool -> Scope -> Scope
insertSym id typeD valNum valBool = modifyTable (modifyScope (M.insert id (Variable typeD valNum valBool)))

-- Verifica si un identificador esta en la tabla de funciones
-- si encuentra el identificador retorna su informacion
findFunc :: SymFunc -> String -> Maybe Function
findFunc symFun id = M.lookup id symFun 

-- Inicia el recorrido del arbol
start :: Init -> RetMonad ()
start (Init funs is) = do
  tell $ S.singleton "Funciones iniciales:"
  mapM_ function funs
  tell $ S.singleton "program"
--  mapM_ instruction is


-- Recorre las funciones iniciales
function :: DefFunc -> RetMonad ()
function input = do
  scope <- get                                                                  -- obtiene todo el estado del monad, lo guarda en scope
  let tableFunc   = func scope                                                  -- obtiene la tabla de los simbolos de las funciones
  let repeatPar   = (Set.size $ Set.fromList ids) /= P.length ids               -- verifica si hay parametros con el mismo identificador
  let isInTable   = M.member id tableFunc                                       -- verifica si el identificador esta en la tabla de simbolos
  case isInTable of
    True -> do return ()                                                        -- errorRepeatFunc id
    False -> do return ()
  case repeatPar of
    True -> do return ()                                                        -- errorRepeatPar par
    False -> do return ()
  modify(modifyFuncT $ M.insert id (Function typeF types))
  modify(modifyTable  addTable)
  modify(modifyTable $ modifyScope $ addSyms ids types)
  scopeFinal <- get
  tell $ S.singleton $ show scopeFinal
  tell $ S.singleton "\n\n"
--  Aca recorrer las intrucciones
  modify(modifyTable eraseLastScope)
  where (identifier, pars, is, typeF)               = getAll input
        getAll (DFun   id' pars' is')               = (id', pars', is', Void)
        getAll (DFunR  id' pars' (TBoolean _) is')  = (id', pars', is', Boolean)
        getAll (DFunR  id' pars' (TNumber  _) is')  = (id', pars', is', Number)      
        (TIdent _ id)                               = identifier                                        -- obtiene identificador de la funcion
        getType (Par (TBoolean _) (TIdent _ id))    = Boolean                     -- funcion para obtener el tipo de un parametro
        getType (Par (TNumber _ ) (TIdent _ id))    = Number                      -- funcion para obtener el tipo de un parametro
        getId   (Par _ (TIdent _ id))               = id                          -- funcion para obtener el tipo de un parametro
        types                                       = P.map getType pars          -- obtener los tipos de los parametros
        ids                                         = P.map getId   pars          -- obtener los identificadores de los parametros
        modifyFuncT f (Scope x symFunc y z)         = Scope x (f symFunc) y z 
        addSyms [] [] symT                          = symT
        addSyms (x:xs) (y:ys) symT                  = M.insert  x (Variable y 0 False) (addSyms xs ys symT)



-- Agrega las declaraciones a la tabla de simbolos
dec :: Dec -> RetMonad ()
dec (Dec1 _ [])                   = return ()
dec (Dec1 t ((TIdent _ id):ds))   = do
  scope <- get
  case (M.member id (head $ sym scope)) of
    True  -> do return ()                                                         -- ERROR ya esta declarada la variable
    False -> do modify(insertSym id (getType t) 0 False)                          -- agregar la nueva variable a la tabla de simbolos
                dec (Dec1 t ds)                                                   -- recursion sobre las otras variables declaradas
  where getType (TBoolean _) = Boolean
        getType (TNumber   _) = Number
dec (Dec2 (TBoolean _) (TIdent _ id) exp) = do
  scope <- get
  case (M.member id (head $ sym scope)) of
    True  -> do return ()                                                         -- ERROR ya esta declarada la variable
    False -> do let val = False                                                   -- Calcular valor de la expresión y tipo
                modify(insertSym id Boolean 0 val)                                -- verificar que coincida el tipo de declaracion
dec (Dec2 (TNumber _) (TIdent _ id) exp) = do
  scope <- get
  case (M.member id (head $ sym scope)) of
    True  -> do return ()                                                         -- ERROR ya esta declarada la variable
    False -> do let val = 0                                                       -- CALCULAR valor de la expresión y tipo
                modify(insertSym id Number val False)                             -- VERIFICAR que coincida el tipo de declaracion 



-- Recorrer un bloque with do
withDo :: Do -> RetMonad ()
withDo (Do decs is) = do
  modify(modifyTable addTable)
  P.mapM_ dec decs
-- P.mapM_  instruction is                                                      -- RECORRER INSTRUCCIONES
  scopeFinal <- get
  tell $ S.singleton $ show scopeFinal
  modify(modifyTable eraseLastScope)                                            -- Eliminar tabla agregada







