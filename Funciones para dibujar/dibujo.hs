module Main(main) where
import System.IO
import Data.Set as Set


----------------------------------------- Tipos de Datos------------------------------
data Point = Point  { x ::  Int
                    , y ::  Int }

data Arc  = Arc {   pos'  :: Point
                  , r     :: Double
                  , alpha :: Double
                  , dir'  :: Double }

data Seg = Seg {  pos   :: Point
                , l     :: Double
                , dir   :: Double }



-- Definicion de operadores entre puntos
instance Num Point where
  (Point x1 y1) + (Point x2 y2) = Point (x1+x2) (y1+y2)       -- Suma de dos Puntos
  (Point x1 y1) - (Point x2 y2) = Point (x1-x2) (y1-y2)       -- Resta de dos Puntos
  (Point x1 y1) * (Point x2 y2) = Point (x1*x2) (y1*y2)
  abs     (Point x y)  = Point (abs x) (abs y)
  signum  (Point x y)  = Point (x `div` (abs x)) (y `div` (abs y))
  fromInteger n        = Point (fromInteger n) (fromInteger n)

-- Producto punto entre puntos
(^*) :: Point -> Point -> Int
(Point x1 y1) ^* (Point x2 y2) = x1*x2 + y1*y2

-- Producto cruz entre puntos
(**) :: Point -> Point -> Int
(Point x1 y1) ** (Point x2 y2) = x1*y2 - y2*x1

-- Distancia entre dos puntos
dist :: Point -> Point -> Double
dist p1 p2 = sqrt $ fromIntegral $ (p1 - p2)^*(p1 - p2)

-- Distancia al cuadrado entre dos puntos
dist2 :: Point -> Point -> Int
dist2 p1 p2 = (p1 - p2)^*(p1 - p2)


-- Angulo de un vector respecto al eje x positivo
ang :: Point -> Double
ang (Point x y) = (positive $ atan2 y' x')*180/pi
  where x' = fromIntegral x
        y' = fromIntegral y
        positive a = if (a < 0) then (2*pi)+a else a 


---- Puntos con coordenadas reales

data PointD = PointD  { x' ::  Double
                      , y' ::  Double }

-- Multiplicacion de un punto con coordenadas reales por un escalar
(*/) :: PointD -> Double -> PointD
(PointD x y) */ a = PointD (x*a) (y*a)

-- Suma de Puntos con coordenadas reales
(+/) :: PointD -> PointD -> PointD
(PointD x1 y1) +/ (PointD x2 y2) = PointD (x1+x2) (y1+y2)

-- Resta de Puntos con coordenadas reales
(-/) :: PointD -> PointD -> PointD
(PointD x1 y1) -/ (PointD x2 y2) = PointD (x1-x2) (y1-y2)

-- Producto punto con coordenadas reales
(^/) :: PointD -> PointD -> Double
(PointD x1 y1) ^/ (PointD x2 y2) = x1*x2 + y1*y2


-- De un punto con coordenadas enteras a uno con coordenadas reales
fromPoint :: Point -> PointD
fromPoint (Point x y) = PointD (fromIntegral x) (fromIntegral y)

-- norma
norma :: PointD -> Double
norma (PointD x y) = sqrt $ x*x + y*y


-- Proyeccion del punto c en el segmento ab
proPointSeg  ::  Point -> PointD -> Point -> PointD
proPointSeg a b' c  
    | d*d < 1         = a'
    | r   < 0         = a'
    | r   > 1         = b'
    | otherwise     = a' +/ ((b' -/ a') */ r)
    where d   = norma (a' -/ b')
          a'  = fromPoint a
          c'  = fromPoint c
          r  = ((c' -/ a') ^/ (b'-/ a')) / (d*d)


-- Distancia entre el segmento ab y el punto c
distPointSeg  ::  Point -> Point -> Point -> Double
distPointSeg a b c = norma ((fromPoint c) -/ proPointSeg a (fromPoint b) c)

---------------------- Verificar si pertenece al objeto deseado -------------------
-- Verifica si un punto esta en el arco descrito
isInArc   :: Point -> Arc -> Bool
isInArc pt (Arc pc r al dir) = 
  if (abs((dist pt pc)-r) <= (sqrt 2)*0.5) && (validAng (pt-pc) inicio fin)  then True else False
      where (inicio, fin) = (dir, dir-al)
            validAng p a b =  (a <= ang p && ang p <= b) ||
                              (b <= ang p && ang p <= a) || 
                              (a <= (ang p) + 360 && (ang p) + 360 <= b) ||
                              (b <= (ang p) - 360 && (ang p) - 360 <= a)
                              
                              
-- Verifica si el punto esta en el segmento descrito
isInSeg :: Seg -> Point -> Bool
isInSeg (Seg pos l dir) p = (distPointSeg pos b p) < (sqrt 2)*0.5
    where 
          al = dir*pi/180
          b  = pos + (Point (floor $ l * (cos al)) (floor $ l * (sin al)))
          
             
--------------------- Dibujar, funciones temporales ---------------------------------
-- Dibujar el arco
drawArc :: Arc -> [[String]]
drawArc  arc =
    reverse [[if  (isInArc (Point x y) arc) then "1 " else "0 " | x <- [0..floor (2*r)] ] | y <- [0..floor (2*r)]]
        where (Arc pt r al dir) = arc


-- Dibujar el segmento
drawSeg :: Seg -> [[String]]
drawSeg  seg = reverse [[if  (isInSeg seg (Point x y)) then "1 " else "0 " | x <- [0..1000] ] | y <- [0..1000]]



---------------------------------------- Crear archivo, temporal ----------------------------
-- Imprimir una linea
printLine :: [String] -> IO()
printLine line = do mapM_ putStr line
                    putStrLn ""


readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x, "")] -> Just x
                                _ -> Nothing
                    


-- Cambiar main' por main para probar circunferencias
main' :: IO ()
main' = do 
      --    putStrLn "Introduzca el radio:"
          line1 <- getLine
      --    putStrLn "Introduzca el angulo (grados):"
          line2 <- getLine
      --    putStrLn "Introduzca la direccion:"
          line3 <- getLine
          let num       = readMaybe line1 :: Maybe Double
          let alpha     = readMaybe line2 :: Maybe Double
          let direccion = readMaybe line3 :: Maybe Double
          let Just r    = num
          let Just al   = alpha
          let Just dir  = direccion
          let c = Point (floor r) (floor r)
          putStrLn "P1"
          putStrLn $ show (2*(x c)+1) ++ " " ++ show (2*(y c)+1)
          mapM_ printLine $ drawArc (Arc c r al dir)
          


-- Cambiar main' por main para probar lineas
main :: IO ()
main   = do
        -- Obtener posicion
        line1 <- getLine    -- obtener x
        line2 <- getLine    -- obtener y
        line3 <- getLine    -- obtener direccion
        line4 <- getLine    -- obtener longitud
        let (Just x, Just y) =  (readMaybe line1 :: Maybe Int, readMaybe line2:: Maybe Int)
        let Just dir        =  readMaybe line3 :: Maybe Double
        let Just l          =  readMaybe line4 :: Maybe Double
        putStrLn "P1\n1001 1001\n"
        mapM_ printLine $ drawSeg (Seg (Point x y) l dir)










