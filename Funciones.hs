-- Recordar que falta el return
-- modificar estructura scope :(

module Funciones where
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
import RetMonad




-- Inicia el recorrido del arbol
start :: Init -> RetMonad ()
start (Init funs is) = do
  tell $ S.singleton "Funciones iniciales:"
  mapM_ function funs
  tell $ S.singleton "program"
  mapM_ instruction is


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
  P.mapM_ instruction is
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
  modify(modifyTable addTable)                                                 -- Crear un nuevo alcance
  P.mapM_ dec decs
  P.mapM_  instruction is                                                      -- RECORRER INSTRUCCIONES
  scopeFinal <- get
  tell $ S.singleton "Bloque Do"
  tell $ S.singleton $ show scopeFinal
  tell $ S.singleton "\n\n"
  modify(modifyTable eraseLastScope)                                            -- Eliminar tabla agregada


-- Recorrer un bloque IF
ifThen :: If -> RetMonad ()
ifThen (If exp is) = do
  tell $ S.singleton "Bloque if"
  let valExp = True                                             -- ACOMODAR CUANDO ESTEN LAS EXPRESIONES
  P.mapM_ instruction is                                        -- Recorrer las instrucciones


-- Recorrer un bloque if else
ifElse :: IfElse -> RetMonad ()
ifElse (IfElse exp is1 is2) = do
  tell $ S.singleton "Bloque if else"
  let val = True                              -- ACOMODAR CUANDO ESTEN LAS EXPRESIONES
  tell $ S.singleton "Cuando es true"
  P.mapM_ instruction is1                     -- Recorrer las instrucciones
  tell $ S.singleton "Cuando es false"
  P.mapM_ instruction is2                     -- Recorrer las instrucciones
  
-- Recorrer un bloque while
while :: While -> RetMonad()
while (While exp is) = do
  tell $ S.singleton "Bloque while"
  let val = True                            -- ACOMODAR CUANDO ESTEN LAS INSTRUCCIONES
  P.mapM_ instruction is                    -- Recorrer las instrucciones



-- Recorrer un bloque repeat
rep :: Repeat -> RetMonad ()
rep (Repeat exp is) = do
  tell $ S.singleton "Bloque repeat"
  let val = True                            -- ACOMODAR CUANDO ESTEN LAS EXPRESIONES
  P.mapM_ instruction is                    -- Recorrer las instrucciones


-- Recorrer un bloque for
for :: For -> RetMonad ()
for (For (TIdent _ id) exp1 exp2 is) = do
  tell $ S.singleton "Bloque for"
  let val1 = 0                              -- ACOMODAR CUANDO SE TENGAN LAS EXPRESIONES
  let val2 = 10                             -- ACOMODAR CUANDO SE TENGAN LAS EXPRESIONES
  modify(modifyTable addTable)              -- agregar nuevo alcance
  modify(insertSym id Number val1 False)    -- agregar el contador a la tabla de simbolos
  P.mapM_ instruction is                    -- Recorrer instrucciones
  modify(modifyTable eraseLastScope)        -- Eliminar tabla agregada


-- Recorrer un bloque forBy
forBy :: ForBy -> RetMonad ()
forBy (ForBy (TIdent _ id) exp1 exp2 exp3 is) = do
  tell $ S.singleton "Bloque for by"
  let val1 = 0                              -- ACOMODAR CUANDO SE TENGAN LAS EXPRESIONES
  let val2 = 10                             -- ACOMODAR CUANDO SE TENGAN LAS EXPRESIONES
  let val2 = 10                             -- ACOMODAR CUANDO SE TENGAN LAS EXPRESIONES
  modify(modifyTable addTable)              -- agregar nuevo alcance
  modify(insertSym id Number val1 False)    -- agregar el contador a la tabla de simbolos
  P.mapM_ instruction is                    -- Recorrer instrucciones
  modify(modifyTable eraseLastScope)        -- Eliminar tabla agregada
  

-- Ejecutar un bloque
block :: Block -> RetMonad ()
block (BDo      ins)  = withDo  ins
block (BIf      ins)  = ifThen  ins
block (BIfElse  ins)  = ifElse  ins
block (BWhile   ins)  = while   ins
block (BFor     ins)  = for     ins
block (BForby   ins)  = forBy   ins
block (BRepeat  ins)  = rep     ins

-- Instruccion read 
readId :: ReadId -> RetMonad ()
readId (ReadId (TIdent _ id)) = do
  scope <- get
  let val = findSym (sym scope) id
  tell $ S.singleton "Instruccion read"
  case (isNothing val) of
    True  -> do return ()        -- ERROR NO ESTA EL IDENTIFICADOR EN LA TABLA
    False -> do return ()

-- Imprimibles
printP :: Print -> RetMonad ()
printP (PToken (TString _ str)) = do return ()
printP (PExp exp)               = do
    let isValid = True    -- TERMINAR CUANDO SE TENGAN EXPRESIONES
    case isValid of
      True  -> do return ()
      False -> do tell $ S.singleton "error en imprimible"


-- instruccion write
writePr :: Write -> RetMonad ()
writePr (Write ps) = do
  tell $ S.singleton "Intruccion write"
  mapM_ printP ps

-- instruccion writeL
writeLPr :: WriteL -> RetMonad ()
writeLPr (WriteL ps) = do
  tell $ S.singleton "Instruccion writeLn"
  mapM_ printP ps
  
-- Ejecutar una instruccion
instruction :: Ins -> RetMonad ()
instruction (IBlock b)  = block b
instruction _           = return ()
