module DrawingMonad where
import System.IO
import Control.Monad.RWS as RWS
import Data.Sequence as S
import Data.Foldable as F
import Drawing


-------------------------------- Tipo de Datos de Instrucciones -----------------------

data Instruction =    InsHome  
                    | InsOpen
                    | InsClose
                    | InsF  Forward 
                    | InsB  Backward
                    | InsRL RotateL
                    | InsRR RotateR
                    | InsS  SetPosition
                    | InsA  ArcD

data Forward      = Forward     Double
data Backward     = Backward    Double
data RotateL      = RotateL     Double
data RotateR      = RotateR     Double
data SetPosition  = SetPosition Double Double
data ArcD         = ArcD        Double Double     -- Grados y Radio

-- Estado de la Tortuga
-- position:  posicion actual de la tortuga
-- angle:     direccion de la tortuga
-- opened:    
data TurtleState = TurtleState  { position :: Point
                                , angle    :: Double
                                , opened   :: Bool }

type DrawMonad = RWS String (S.Seq(Line)) TurtleState

------------- Funciones para manejar el State --------------------

-- Funcion para configurar el ojo de la tortuga
setOpenEye :: Bool -> TurtleState -> TurtleState
setOpenEye b (TurtleState pos dir _) = TurtleState pos dir b 

--Funcion para configurar la posicion
setPosition :: (Point) -> TurtleState -> TurtleState
setPosition pos (TurtleState _ dir b) = TurtleState pos dir b

-- Funcion para modificar la direccion
modifyDir :: (Double -> Double) -> TurtleState -> TurtleState
modifyDir f (TurtleState pos dir b) = TurtleState pos (f dir) b

-- Funcion para cambiar la posicion de la tortuga
move :: Double -> TurtleState -> TurtleState
move len (TurtleState pos al b) = TurtleState (pos + (Point (len* (cos $ radians al) ) (len*(sin $ radians al)))) al b
                                    where radians x = x*pi/180



----------------------------------- Funciones de dibujo y movimiento ---------------------------------------------------
-- Funcion forward
forward :: Forward -> DrawMonad ()
forward (Forward len) = do  turtle  <- get                                                -- Obtener el estado de la tortuga
                            let (pos, alpha) = (position turtle, angle turtle)            -- Obtener la posicion y angula de la tortuga
                            case opened turtle of                                        
                              True    -> tell $ singleton (Right (Seg pos len alpha))     -- Cuando la tortuga tenga el ojo abierto agregar la linea
                              False   -> return ()                                        -- Cuando no este abierto, ser feliz
                            modify (move len)                                             -- Cambiar de posicion a la tortuguita

-- Funcion backward: funcion forward con la longitud negativa
backward :: Backward -> DrawMonad ()
backward (Backward len) = forward (Forward (-len))

-- Funcion para rotar a la tortuga, es decir, cambiar su direccion
rotate :: Double -> Double -> Double
rotate x a = (a+x) - fromIntegral (360*(floor ((a+x)/360.00)))

-- Funcion para agregar un nuevo arco las lineas
addArc :: ArcD -> DrawMonad ()
addArc (ArcD radio alpha) = do  turtle  <- get
                                let (dir, pos) = (angle turtle, position turtle)
                                tell $ singleton (Left (Arc pos radio alpha dir))
                                return ()

--- Ejecutar una instruccion de dibujo
instructionD :: Instruction -> DrawMonad ()
instructionD InsHome                    = do modify(setPosition (Point 0 0))          -- Devuelve a la tortuga a la posicion inicial
instructionD InsOpen                    = do modify(setOpenEye True)                  -- Todo el movimiento es marcado
instructionD InsClose                   = do modify(setOpenEye False)                 -- Ningun movimiento es marcado
instructionD (InsF  insForward)         = do forward   insForward                     -- Avanza
instructionD (InsB  insBackward)        = do backward  insBackward                    -- Retrocede             
instructionD (InsRL (RotateL x))        = do modify(modifyDir (rotate   x ))          -- Rota a la izquierda
instructionD (InsRR (RotateR x))        = do modify(modifyDir (rotate (-x)))          -- Rota a la derecha
instructionD (InsS  (SetPosition x y))  = do modify((setPosition (Point x y)))        -- Modifica la posicion de la tortuga
instructionD (InsA  arc)                = do addArc arc                               -- Dibuja un arco


-- Estado inicial de la tortuga
turtleInitial :: TurtleState
turtleInitial = TurtleState (Point 0 0) 90 True


-- Obtener el ouput
-- file: es el nombre del archivo que se creara
-- ls: Lista de instrucciones de dibujo que deben ser procesadas
-- Devuelve una lista de String, crea la imagen correspondiente
-- a la lista de instrucciones en un archivo de nombre file.pbm
image :: FilePath -> [Instruction] -> IO ()
image file ls = do  let (s, lines) = execRWS (RWS.mapM_ instructionD ls) "" turtleInitial
                    let fileOut = file ++ ".pbm"
                    writeFile fileOut "P1\n1001\n1001\n"
                    appendFile fileOut (unlines $ draw $ F.toList lines)



------------- PRUEBA, ELIMINAR PARA LA ENTREGA ---------------------
-- Instructiones de prueba
prueba :: [Instruction]
prueba =  [ InsF (Forward 100),
            InsRR (RotateR 90),
            InsF (Forward 100), 
            InsRR (RotateR 90),
            InsF (Forward 100),
            InsRR (RotateR 90),
            InsF (Forward 100),
            InsRR (RotateR 135),
            InsF  (Forward 141)
          ] 

--main = image "prueba" prueba
