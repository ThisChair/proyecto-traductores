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
  modify(modifyFuncT $ M.insert "home" (Function Void [] [] []))
  modify(modifyFuncT $ M.insert "openeye" (Function Void [] [] []))
  modify(modifyFuncT $ M.insert "closeeye" (Function Void [] [] []))
  modify(modifyFuncT $ M.insert "forward" (Function Void ["n"] [Number] []))
  modify(modifyFuncT $ M.insert "backward" (Function Void ["n"] [Number] []))
  modify(modifyFuncT $ M.insert "rotatel" (Function Void ["n"] [Number] []))
  modify(modifyFuncT $ M.insert "rotater" (Function Void ["n"] [Number] []))
  modify(modifyFuncT $ M.insert "setposition" (Function Void ["x","y"] [Number,Number] []))
  modify(modifyFuncT $ M.insert "arc" (Function Void ["n","r"] [Number,Number] []))
  mapM_ function funs
  modify(changeName "_noFunction")                            -- Cambiar el nombre de la funcion
  modify(changeTypeRet Void)                                  -- Cambiar el tipo de retorno
  modify(changeTypeScope IsProgram)                           -- Cambiar el tipo de alcance
  modify(modifyTable addTable)                                -- Añadir una nueva tabla de simbolos
  scopeFinal <- get                                           -- Obtener el alcance final
  P.mapM_ instruction is
  where modifyFuncT f (Scope x symFunc v w ts fr)  = Scope x (f symFunc) v w ts fr


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
        modifyFuncT f (Scope x symFunc v w ts fr)  = Scope x (f symFunc) v w ts fr



-- Agrega las declaraciones a la tabla de simbolos
dec :: Dec -> RunMonad ()
dec (Dec1 _ [])                   = return ()                                     -- Lista vacía, termina la recursión 
dec (Dec1 t ((TIdent p id):ds))   = do   
  scope <- get                                                                    -- obtener el alcance actual
  modify(insertSym id (Variable (getType t) 0 False True))                        -- Agregar la nueva variable a la tabla de simbolos
  dec (Dec1 t ds)
  where getType (TBoolean _) = Boolean
        getType (TNumber  _) = Number
dec (Dec2 typeD (TIdent p id) exp) = do                                           -- segundo tipo de declaracion, con asignacion
  scope <- get                                                                    -- obtener el alcance
  var <- runExpress exp                                                           -- EXPRESION
  modify(insertSym id var) 



-- Recorrer un bloque with do
withDo :: Do -> RunMonad ()
withDo (Do decs is) = do
  modify(modifyTable addTable)                                                  -- Crear un nuevo alcance
  modify(changeTypeScope IsWithDo)                                              -- Cambiar el tipo de alcance
  P.mapM_ dec decs                                                              -- Verificar que las declaraciones sean correctas
  scopeFinal <- get
  P.mapM_  instruction is                                                       -- RECORRER INSTRUCCIONES
  modify(modifyTable eraseLastScope)                                            -- Eliminar tabla agregada


-- Recorrer un bloque IF
ifThen :: If -> RunMonad ()
ifThen (If exp is) = do
  valExp <- runExpress exp                                         -- Calcular la expresion condicional
  case (bool valExp) of
    True -> P.mapM_ instruction is
    False -> return()

-- Recorrer un bloque if else
ifElse :: IfElse -> RunMonad ()
ifElse (IfElse exp is1 is2) = do
  valExp <- runExpress exp                                       -- Calcular la expresion condicional
  case (bool valExp) of
    True -> P.mapM_ instruction is1
    False -> P.mapM_ instruction is2
  
-- Recorrer un bloque while
while :: While -> RunMonad()
while (While exp is) = do
  runWhile exp is                                       -- Calcular la expresion condicional



-- Recorrer un bloque repeat
rep :: Repeat -> RunMonad ()
rep (Repeat exp is) = do
  valExp <- runExpress exp                                       -- Calcular la expresion numerica
  let intExp = floor (num valExp)
  runRepeat intExp is

-- Recorrer un bloque for
for :: For -> RunMonad ()
for (For (TIdent p id) exp1 exp2 is) = do

  val1 <- runExpress exp1                                        -- Calcular expresión inicial
  val2 <- runExpress exp2                                        -- Calcular expresión final

  let fVal1 = (Variable Number (fromInteger $ floor (num val1)) False False)
  let fVal2 = (Variable Number (fromInteger $ floor (num val2)) False True)
  -- Si todo esta bien, entonces continuar
  modify(modifyTable addTable)                                -- agregar nuevo alcance
  modify(insertSym id fVal1)                                  -- agregar el contador a la tabla de simbolos
  modify(changeTypeScope IsFor)                               -- Cambiar el tipo de alcance
  scopeFinal <- get
  runFor id fVal2 is
  modify(modifyTable eraseLastScope)                          -- Eliminar tabla agregada


-- Recorrer un bloque forBy
forBy :: ForBy -> RunMonad ()
forBy (ForBy (TIdent p id) exp1 exp2 exp3 is) = do

  val1 <- runExpress exp1                                        -- Calcular expresion inicial
  val2 <- runExpress exp2                                        -- Calcular expresion final
  val3 <- runExpress exp3                                        -- Calcular expresion de salto

  modify(modifyTable addTable)                                   -- agregar nuevo alcance
  modify(insertSym id (Variable Number (num val1) False False))  -- agregar el contador a la tabla de simbolos
  modify(changeTypeScope IsForBy)                                -- Cambiar el tipo de alcance
  scopeFinal <- get                                              -- Obtener el alcance final
  runForBy id val2 val3 is                                       -- Recorrer instrucciones
  modify(modifyTable eraseLastScope)                             -- Eliminar tabla agregada
  
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
  let var = fromJust (findSym (sym scope) id)
  val <- liftIO $ getLine
  case (mutable var) of
    False -> do return () -- Error ):
    True -> do
      case (t var) of
        Boolean -> do
          case val of
            "true" -> do modify(modifyTable $ modifySym id (Variable Boolean 0 True True))
            "false" -> do modify(modifyTable $ modifySym id (Variable Boolean 0 False True))
            _ -> do return () --Error ):
        Number -> do
          let n = readMaybe val
          case n of
            Nothing -> return() -- Error ):
            Just x -> modify(modifyTable $ modifySym id (Variable Number x False True))
  return ()

------- Read Maybe
readMaybe :: String -> Maybe Double
readMaybe st = case reads st of [(x, "")] -> Just x
                                _ -> Nothing

-- Imprimibles
printP :: Print -> RunMonad ()
printP (PToken (TString _ str)) = do liftIO . putStr $ str
printP (PExp exp)               = do
  val <- runExpress exp                                -- Calcular valor de la expresion
  case (t val) of
    Number  -> do liftIO . putStr $ show (num val)
    Boolean -> do liftIO . putStr $ show (bool val)

-- instruccion write
writePr :: Write -> RunMonad ()
writePr (Write ps) = do
  mapM_ printP ps
  liftIO . putStr $ ""

-- instruccion writeL
writeLPr :: WriteL -> RunMonad ()
writeLPr (WriteL ps) = do
  mapM_ printP ps
  liftIO . putStrLn $ ""


-- instruccion de asignacion
assig :: Assig -> RunMonad ()
assig (Assig (TIdent p id) exp) = do
  scope   <- get
  let var = fromJust (findSym (sym scope) id)
  case (mutable var) of
    False -> do return () --Error ):
    True -> do
      valExp  <- runExpress exp                                      -- Calcular valor de la expresion
      modify(modifyTable $ modifySym id valExp)                      -- modificar la variable


-- instrucion de retorno
returnIns :: Ret -> RunMonad ()
returnIns (Ret exp) = do
  scope <- get
  modify(changeFoundR True)                                     -- Indica que se encontró una expresión de retorno
  let typeReturn = typeRet scope
  val <- runExpress exp                                            -- Calcular expresion
  return()



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

-- Funciones recursivas para ciclos

runWhile :: Exp -> [Ins] -> RunMonad ()
runWhile cond is = do
  valExp <- runExpress cond
  case (bool valExp) of
    True -> do 
      P.mapM_ instruction is
      runWhile cond is
    False -> do return()
    
runRepeat :: Integral a => a -> [Ins] -> RunMonad ()
runRepeat n is = do
  case n of
    0 -> do return()
    _ -> do 
      P.mapM_ instruction is
      runRepeat (n-1) is

runFor :: String -> Variable -> [Ins] -> RunMonad()
runFor id quota is = do
  scope <- get
  let tableSym  = sym scope
  let var = fromJust (findSym tableSym id)
  let counter = num var
  case (counter > num quota) of
    True -> do return()
    False -> do 
      P.mapM_ instruction is
      modify(modifyTable $ modifySym id (Variable Number (counter + 1) False False))
      runFor id quota is

runForBy :: String -> Variable -> Variable -> [Ins] -> RunMonad()
runForBy id quota inc is = do
  scope <- get
  let tableSym  = sym scope
  let var = fromJust (findSym tableSym id)
  let counter = num var
  case (counter > num quota) of
    True -> do return()
    False -> do 
      P.mapM_ instruction is
      modify(modifyTable $ modifySym id (Variable Number (counter + (num inc)) False False))
      runForBy id quota inc is