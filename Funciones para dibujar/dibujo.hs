module Main(main) where
import System.IO
import Data.Set as Set


----------------------------------------- Point ------------------------------
data Point = Point  { x ::  Int
                    , y ::  Int }

data Arc  = Arc {   pos   :: Point
                  , r     :: Double
                  , alpha :: Double
                  , dir   :: Double}


-- Definicion de operadores entre puntos
instance Num Point where
  (Point x1 y1) + (Point x2 y2) = Point (x1+x2) (y1+y2)       -- Suma de dos Puntos
  (Point x1 y1) - (Point x2 y2) = Point (x1-x2) (y1-y2)       -- Resta de dos Puntos
  (Point x1 y1) * (Point x2 y2) = Point (x1*x2) (y1*y2)
  abs     (Point x y)  = Point (abs x) (abs y)
  signum  (Point x y)  = Point (x `div` (abs x)) (y `div` (abs y))
  fromInteger n        = Point (fromInteger n) (fromInteger n)

-- Funcion producto punto entre puntos
(^*) :: Point -> Point -> Int
(Point x1 y1) ^* (Point x2 y2) = x1*x2 + y1*y2

-- Distancia entre dos puntos
dist :: Point -> Point -> Double
dist p1 p2 = sqrt $ fromIntegral $ (p1 - p2)^*(p1 - p2)


-- Angulo de un vector respecto al eje x positivo
ang :: Point -> Double
ang (Point x y) = (positive $ atan2 y' x')*180/pi
  where x' = fromIntegral x
        y' = fromIntegral y
        positive a = if (a < 0) then (2*pi)+a else a 

-- Verifica si un punto está en el arco descrito
isInArc   :: Point -> Arc -> Bool
isInArc pt (Arc pc r al dir) = 
  if (abs((dist pt pc)-r) <= (sqrt 2)*0.5) && (validAng (pt-pc) inicio fin)  then True else False
      where (inicio, fin) = (dir, dir-al)
            validAng p a b =  (a <= ang p && ang p <= b) ||
                              (b <= ang p && ang p <= a) || 
                              (a <= (ang p) + 360 && (ang p) + 360 <= b) ||
                              (b <= (ang p) - 360 && (ang p) - 360 <= a)
              
-- Dibujar el arco
drawArc :: Arc -> [[String]]
drawArc  arc =
    reverse [[if  (isInArc (Point x y) arc) then "1 " else "0 " | x <- [0..floor (2*r)] ] | y <- [0..floor (2*r)]]
        where (Arc pt r al dir) = arc

-- Imprimir una linea
printLine :: [String] -> IO()
printLine line = do mapM_ putStr line
                    putStrLn ""
  
main :: IO ()
main = do 
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
          

readMaybe :: (Read a) => String -> Maybe a
readMaybe st = case reads st of [(x, "")] -> Just x
                                _ -> Nothing











