-- Recordar que falta el return
-- modificar estructura scope :(

module RunFunciones where
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
import RunMonad
import RunExpress



-- Inicia el recorrido del arbol
start :: Init -> RunMonad ()
start (Init funs is) = do
  modify(modifyFuncT $ M.insert "home" (Function Void []))
  modify(modifyFuncT $ M.insert "openeye" (Function Void []))
  modify(modifyFuncT $ M.insert "closeeye" (Function Void []))
  modify(modifyFuncT $ M.insert "forward" (Function Void [Number]))
  modify(modifyFuncT $ M.insert "backward" (Function Void [Number]))
  modify(modifyFuncT $ M.insert "rotatel" (Function Void [Number]))
  modify(modifyFuncT $ M.insert "rotater" (Function Void [Number]))
  modify(modifyFuncT $ M.insert "setposition" (Function Void [Number,Number]))
  modify(modifyFuncT $ M.insert "arc" (Function Void [Number,Number]))
  mapM_ function funs
  modify(changeName "_noFunction")                            -- Cambiar el nombre de la funcion
  modify(changeTypeRet Void)                                  -- Cambiar el tipo de retorno
  modify(modifyCounter plusOne)                               -- Agregar 1 al contador
  modify(changeTypeScope IsProgram)                           -- Cambiar el tipo de alcance
  modify(modifyTable  addTable)                               -- Añadir una nueva tabla de simbolos
  scopeFinal <- get                                           -- Obtener el alcance final
  tell $ S.singleton $ scopeFinal
  modify(modifyCounter addCounter)                            -- Agregar nuevo contador
  P.mapM_ instruction is
  where modifyFuncT f (Scope x symFunc y z v w ts fr)  = Scope x (f symFunc) y z v w ts fr


-- Recorre las funciones iniciales
function :: DefFunc -> RunMonad ()
function input = do
  scope <- get                                                                  -- obtiene todo el estado del monad, lo guarda en scope
  let tableFunc   = func scope                                                  -- obtiene la tabla de los simbolos de las funciones
  modify(changeName id)                                                         -- Cambiar el nombre del identificador de la funcion
  modify(changeTypeRet typeF)                                                   -- Cambiar el tipo de retorno de la funcion
  modify(changeTypeScope IsFun)                                                 -- Cambiar el tipo de alcance
  modify(modifyFuncT $ M.insert id (Function typeF ids types is))               -- Agregar el identificador a la tabla de simbolos
  scopeFinal <- get
  modify(modifyTable eraseLastScope)                                            -- Eliminar ultimo alcance
  where (identifier, pars, is, typeF)               = getAll input
        getAll (DFun   id' pars' is')               = (id', pars', is', Void)
        getAll (DFunR  id' pars' (TBoolean _) is')  = (id', pars', is', Boolean)
        getAll (DFunR  id' pars' (TNumber  _) is')  = (id', pars', is', Number)
        (TIdent _ id)                               = identifier                  -- obtiene identificador de la funcion      
        getType (Par (TBoolean _) (TIdent _ id))    = Boolean                     -- funcion para obtener el tipo de un parametro
        getType (Par (TNumber _ ) (TIdent _ id))    = Number                      -- funcion para obtener el tipo de un parametro
        getId   (Par _ (TIdent _ id''))             = id''                        -- funcion para obtener el identificador de un parametro
        types                                       = P.map getType pars          -- obtener los tipos de los parametros
        ids                                         = P.map getId   pars          -- obtener los identificadores de los parametros
        modifyFuncT f (Scope x symFunc y z v w ts fr)  = Scope x (f symFunc) y z v w ts fr



-- Agrega las declaraciones a la tabla de simbolos
dec :: Dec -> RunMonad ()
dec (Dec1 _ [])                   = return ()                                     -- Lista vacía, termina la recursión 
dec (Dec1 t ((TIdent p id):ds))   = do   
  scope <- get                                                                    -- obtener el alcance actual
  modify(insertSym id (Variable (getType t) 0 False))                             -- Agregar la nueva variable a la tabla de simbolos
  dec (Dec1 t ds)
  where getType (TBoolean _) = Boolean
        getType (TNumber  _) = Number
dec (Dec2 typeD (TIdent p id) exp) = do                                           -- segundo tipo de declaracion, con asignacion
  scope <- get                                                                    -- obtener el alcance
  var <- runExpress exp                                                              -- EXPRESION
  modify(insertSym id var) 



-- Recorrer un bloque with do
withDo :: Do -> RunMonad ()
withDo (Do decs is) = do
  modify(modifyHeight (+1))                                                     -- Sumar uno a la altura
  modify(modifyCounter plusOne)                                                 -- Incrementar en uno el contador actual
  modify(modifyTable addTable)                                                  -- Crear un nuevo alcance
  modify(changeTypeScope IsWithDo)                                              -- Cambiar el tipo de alcance
  P.mapM_ dec decs                                                              -- Verificar que las declaraciones sean correctas
  scopeFinal <- get
  modify(modifyCounter addCounter)                                              -- Agregar nuevo contador
  P.mapM_  instruction is                                                       -- RECORRER INSTRUCCIONES
  modify(modifyTable eraseLastScope)                                            -- Eliminar tabla agregada
  modify(modifyCounter eraseCounter)                                            -- Eliminar ultimo contador
  modify(modifyHeight (+(-1)))                                                  -- Restar uno a la altura


-- Recorrer un bloque IF
ifThen :: If -> RunMonad ()
ifThen (If exp is) = do
  valExp <- runExpress exp                                         -- Calcular la expresion condicional
  case (t valExp) of
    Number  -> do errUnexpectedType exp Number Boolean          -- ERROR LA EXPRESION DEBERIA SER BOOLEANA
    Boolean -> do P.mapM_ instruction is                        -- Recorrer las instrucciones


-- Recorrer un bloque if else
ifElse :: IfElse -> RunMonad ()
ifElse (IfElse exp is1 is2) = do
--  tell $ S.singleton "Bloque if else"
  valExp <- runExpress exp                                       -- Calcular la expresion condicional
  case (t valExp) of
    Number  -> do errUnexpectedType exp Number Boolean        -- ERROR LA EXPRESION DEBERIA SER BOOLEANA
    Boolean -> do P.mapM_ instruction is1                     -- Recorrer las instrucciones
                  P.mapM_ instruction is2                     -- Recorrer las instrucciones
  
-- Recorrer un bloque while
while :: While -> RunMonad()
while (While exp is) = do
--  tell $ S.singleton "Bloque while"
  valExp <- runExpress exp                                       -- Calcular la expresion condicional
  case (t valExp) of
    Number  -> do errUnexpectedType exp Number Boolean        -- ERROR LA EXPRESION DEBERIA SER BOOLEANA
    Boolean -> do P.mapM_ instruction is                      -- Recorrer las instrucciones



-- Recorrer un bloque repeat
rep :: Repeat -> RunMonad ()
rep (Repeat exp is) = do
--  tell $ S.singleton "Bloque repeat"
  valExp <- runExpress exp                                       -- Calcular la expresion numerica
  case (t valExp) of
    Boolean  -> do errUnexpectedType exp Boolean Number       -- ERROR LA EXPRESION DEBERIA SER NUMERICA
    Number   -> do P.mapM_ instruction is                     -- Recorrer las instrucciones


-- Recorrer un bloque for
for :: For -> RunMonad ()
for (For (TIdent p id) exp1 exp2 is) = do
  case (inExp (TIdent p id) exp1) of
    True -> do errForVar (TIdent p id) exp1                        -- ERROR VARIABLE SE USA EN LA EXPRESIÓN DE CICLO
    False -> do return ()
  case (inExp (TIdent p id) exp2) of
    True -> do errForVar (TIdent p id) exp2                         -- ERROR VARIABLE SE USA EN LA EXPRESIÓN DE CICLO
    False -> do return ()
  val1 <- runExpress exp1                                        -- Calcular expresión inicial
  val2 <- runExpress exp2                                        -- Calcular expresión final
  case (t val1) of                                            -- Verificar que la expresion inicial sea numerica 
    Number  -> do return ()
    Boolean -> do errUnexpectedType exp1 Boolean Number       -- ERROR LA EXPRESIÓN DEBERIA SER NUMÉRICA
  case (t val2) of                                            -- Verificar que la expresion final sea numerica
    Number  -> do return ()                                 
    Boolean -> do errUnexpectedType exp2 Boolean Number       -- ERROR LA EXPRESIÓN DEBERIA SER NUMÉRICA
  -- Si todo esta bien, entonces continuar
  modify(modifyTable addTable)                                -- agregar nuevo alcance
  modify(insertSym id val1)                                   -- agregar el contador a la tabla de simbolos
  modify(modifyCounter plusOne)                               -- Incrementar en uno el contador actual
  modify(modifyHeight (+1))                                   -- Sumar uno a la altura
  modify(changeTypeScope IsFor)                               -- Cambiar el tipo de alcance
  scopeFinal <- get
--  tell $ S.singleton "Bloque For"
  tell $ S.singleton $ scopeFinal
  modify(modifyCounter addCounter)                            -- Agregar nuevo contador
  P.mapM_ instruction is                                      -- Recorrer instrucciones
  modify(modifyTable eraseLastScope)                          -- Eliminar tabla agregada
  modify(modifyCounter eraseCounter)                          -- Eliminar contador
  modify(modifyHeight (+(-1)))                                -- restar uno a la altura


-- Recorrer un bloque forBy
forBy :: ForBy -> RunMonad ()
forBy (ForBy (TIdent p id) exp1 exp2 exp3 is) = do
--  tell $ S.singleton "Bloque for by"
  case (inExp (TIdent p id) exp1) of
    True -> do errForVar (TIdent p id) exp1                        -- ERROR VARIABLE SE USA EN LA EXPRESIÓN DE CICLO
    False -> do return ()
  case (inExp (TIdent p id) exp2) of
    True -> do errForVar (TIdent p id) exp2                         -- ERROR VARIABLE SE USA EN LA EXPRESIÓN DE CICLO
    False -> do return ()
  case (inExp (TIdent p id) exp3) of
    True -> do errForVar (TIdent p id) exp3                         -- ERROR VARIABLE SE USA EN LA EXPRESIÓN DE CICLO
    False -> do return ()
  val1 <- runExpress exp1                                        -- Calcular expresion inicial
  val2 <- runExpress exp2                                        -- Calcular expresion final
  val3 <- runExpress exp3                                        -- Calcular expresion de salto
  case (t val1) of                                            -- Verificar que la expresion inicial sea numerica 
    Number  -> do return ()
    Boolean -> do errUnexpectedType exp1 Boolean Number       -- ERROR LA EXPRESION DEBERIA SER NUMERICA
  case (t val2) of                                            -- Verificar que la expresion final sea numerica
    Number  -> do return ()                                 
    Boolean -> do errUnexpectedType exp2 Boolean Number       -- ERROR LA EXPRESION DEBERIA SER NUMERICA
  case (t val3) of                                            -- Verificar que la expresion final sea numerica
    Number  -> do return ()                                 
    Boolean -> do errUnexpectedType exp3 Boolean Number       -- ERROR LA EXPRESION DEBERIA SER NUMERICA
  -- Si todo esta bien, entonces continuar
  modify(modifyTable addTable)                                -- agregar nuevo alcance
  modify(insertSym id val1)                                   -- agregar el contador a la tabla de simbolos
  modify(modifyCounter plusOne)                               -- Incrementar en uno el contador actual
  modify(modifyHeight (+1))                                   -- Sumar uno a la altura
  modify(changeTypeScope IsForBy)                             -- Cambiar el tipo de alcance
  scopeFinal <- get                                           -- Obtener el alcance final
--  tell $ S.singleton "Bloque For"
  tell $ S.singleton $ scopeFinal
  modify(modifyCounter addCounter)                            -- Agregar nuevo contador
  P.mapM_ instruction is                                      -- Recorrer instrucciones
  modify(modifyTable eraseLastScope)                          -- Eliminar tabla agregada
  modify(modifyCounter eraseCounter)                          -- Eliminar contador
  modify(modifyHeight (+(-1)))                                -- restar uno a la altura
  
-- Ejecutar un bloque
block :: Block -> RunMonad ()
block (BDo      ins)  = withDo  ins
block (BIf      ins)  = ifThen  ins
block (BIfElse  ins)  = ifElse  ins
block (BWhile   ins)  = while   ins
block (BFor     ins)  = for     ins
block (BForby   ins)  = forBy   ins
block (BRepeat  ins)  = rep     ins

-- Instruccion read 
readId :: ReadId -> RunMonad ()
readId (ReadId (TIdent p id)) = do
  scope <- get
  let val = findSym (sym scope) id
--  tell $ S.singleton "Instruccion read"
  case (isNothing val) of
    True  -> do errNotDeclared (TIdent p id)                                -- ERROR VARIABLE NO DECLARADA
    False -> do return ()

-- Imprimibles
printP :: Print -> RunMonad ()
printP (PToken (TString _ str)) = do return ()
printP (PExp exp)               = do
    val <- runExpress exp                                -- Calcular valor de la expresion
    case (t val) of
      Number  -> do return ()
      Boolean -> do return ()

-- instruccion write
writePr :: Write -> RunMonad ()
writePr (Write ps) = do
--  tell $ S.singleton "Intruccion write"
  mapM_ printP ps

-- instruccion writeL
writeLPr :: WriteL -> RunMonad ()
writeLPr (WriteL ps) = do
--  tell $ S.singleton "Instruccion writeLn"
  mapM_ printP ps


-- instruccion de asignacion
assig :: Assig -> RunMonad ()
assig (Assig (TIdent p id) exp) = do
--  tell $ S.singleton "instruccion de asignacion"
  scope   <- get
  valExp  <- runExpress exp                                      -- Calcular valor de la expresion
  case (findSym (sym scope) id) of                            -- Verificar que la variable este declarada
    Nothing   -> do errNotDeclared (TIdent p id)              -- ERROR VARIABLE NO DECLARADA
    Just var  -> do
      case (t var /= t valExp) of
        True -> do errUnexpectedType (EToken (TIdent p id)) (t var) (t valExp) -- ERROR EN TIPO DE LA VARIABLE
        False -> do modify(modifyTable $ modifySym id var)    -- modificar la variable


-- instrucion de retorno
returnIns :: Ret -> RunMonad ()
returnIns (Ret exp) = do
--  tell $ S.singleton "instruccion de retorno"
  scope <- get
  modify(changeFoundR True)                                     -- Indica que se encontró una expresión de retorno
  let typeReturn = typeRet scope
  val <- runExpress exp                                            -- Calcular expresion
  case typeReturn of                                            -- Verificar que se este en una funcion que devuelva un valor
    Void  -> do errUnexpectedReturn exp                         -- ERROR, NO SE ENCUENTRA EN UN ALCANCE CON VALOR DE RETORNO
    _     -> do return ()
  case (typeReturn == (t val)) of
    False  -> do errUnexpectedType exp (t val) typeReturn        -- ERROR, SE ESPERA UNA EXPRESION DEL TIPO 'typeRet'
    True -> do return ()



-- Ejecutar una instruccion
-- Falta llamada a funcion e instruccion de retorno
instruction :: Ins -> RunMonad ()
instruction (IBlock   ins)  = block     ins
instruction (IReadId  ins)  = readId    ins
instruction (IWrite   ins)  = writePr   ins
instruction (IWriteL  ins)  = writeLPr  ins
instruction (IAssig   ins)  = assig     ins
instruction (IRet     ins)  = returnIns ins
instruction (IFCall   ins)  = do  f <- funcCall  ins         -- ASEGURAR QUE SERA LLAMADA LA FUNCION
                                  return ()
instruction IEmpty          = do return ()
