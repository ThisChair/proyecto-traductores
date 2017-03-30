module Main(main) where
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
                    | InsS  SetPosicion
                    | InsA  ArcD

data Forward      = Forward     Double
data Backward     = Backward    Double
data RotateL      = RotateL     Double
data RotateR      = RotateR     Double
data SetPosicion  = SetPosicion Double Double
data ArcD         = ArcD        Double Double     -- Grados y Radio

data TurtleState = TurtleState  { position :: Point
                                , angle    :: Double
                                , opened   :: Bool }

type DrawMonad = RWS String (S.Seq(Line)) TurtleState

------------- Funciones para manejar el State --------------------

-- Funcion para modificar la posicion
modifyPosition :: (Point -> Point) -> TurtleState -> TurtleState
modifyPosition f (TurtleState pos dir b) = TurtleState (f pos) dir b

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
move len (TurtleState pos al b) = TurtleState (pos + (Point (len*cos(al)) (len*sin(al)))) al b



----------------------------------- Funciones del Monad ---------------------------------------------------
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
instructionD InsHome                    = do modify(setPosition (Point 0 0))
instructionD (InsOpen)                  = do modify(setOpenEye True)
instructionD (InsClose)                 = do modify(setOpenEye False)
instructionD (InsF  insForward)         = do forward   insForward
instructionD (InsB  insBackward)        = do backward  insBackward                                          
instructionD (InsRL (RotateL x))        = do modify(modifyDir (rotate x))
instructionD (InsRR (RotateR x))        = do modify(modifyDir (rotate (-x)))
instructionD (InsS  (SetPosicion x y))  = do modify((setPosition (Point x y)))
instructionD (InsA  arc)                = do addArc arc


-- Estado inicial de la tortuga
turtleInitial :: TurtleState
turtleInitial = TurtleState (Point 0 0) 90 True


-- Obtner el ouput
getBits :: [Instruction] -> [String]
getBits ls = draw $ F.toList lines
    where (w, lines) = execRWS (RWS.mapM_ instructionD ls) "" turtleInitial

main = do putStrLn "OK"
