module Drawing where
import System.IO
import Control.Monad.State




----------------------------------------- Tipos de Datos------------------------------

data Point = Point  { x ::  Double        -- Coordenada x
                    , y ::  Double }      -- Coordenada y

data Arc  = Arc {   pos   :: Point        -- Posicion inicial, centro del arco    
                  , r     :: Double       -- Radio del arco
                  , alpha :: Double       -- Angulo de giro
                  , dir   :: Double }     -- Direccion inicial del arco

data Seg = Seg {  pos'  :: Point          -- Posicion inicial, punto inicial del segmento
                , l     :: Double         -- Longitud del segmento
                , dir'  :: Double }       -- Direccion: angulo del segmento respecto al eje x


-- Constructor de Point usando un par de enteros
fromPair :: (Int, Int) -> Point
fromPair (x, y) = Point (fromIntegral x) (fromIntegral y)

-- Definicion de operadores entre puntos
instance Num Point where
  (Point x1 y1) + (Point x2 y2) = Point (x1+x2) (y1+y2)       -- Suma de dos Puntos
  (Point x1 y1) - (Point x2 y2) = Point (x1-x2) (y1-y2)       -- Resta de dos Puntos
  (Point x1 y1) * (Point x2 y2) = Point (x1*x2) (y1*y2)
  abs     (Point x y)  = Point (abs x) (abs y)
  signum  (Point x y)  = Point (x / (abs x)) (y / (abs y))
  fromInteger n        = Point (fromInteger n) (fromInteger n)

-- Producto punto entre puntos
(^*) :: Point -> Point -> Double
(Point x1 y1) ^* (Point x2 y2) = x1*x2 + y1*y2

-- Producto cruz entre puntos
(**) :: Point -> Point -> Double
(Point x1 y1) ** (Point x2 y2) = x1*y2 - y2*x1

-- Multiplicacion de un punto por un escalar
(*/) :: Point -> Double -> Point
(Point x y) */ a = Point (x*a) (y*a)

-- Distancia al cuadrado entre dos puntos
dist2 :: Point -> Point -> Double
dist2 p1 p2 = (p1 - p2)^*(p1 - p2)

-- Distancia entre dos puntos
dist :: Point -> Point -> Double
dist p1 p2 = sqrt $  dist2 p1 p2

-- Angulo de un vector respecto al eje x positivo
ang :: Point -> Double
ang (Point x y) = (positive $ atan2 y x)*180/pi
  where positive a = if (a < 0) then (2*pi)+a else a 


-- Proyeccion del punto c en el segmento ab
proPointSeg  ::  Point -> Point -> Point -> Point
proPointSeg a b c  
    | d*d < 1         = a
    | r   < 0         = a
    | r   > 1         = b
    | otherwise     = a + ((b-a) */ r)
    where d   = dist2 a b
          r  = ((c - a)^*(b-a)) / d


-- Distancia entre el segmento ab y el punto c
distPointSeg  ::  Point -> Point -> Point -> Double
distPointSeg a b c = dist c (proPointSeg a b c)

---------------------- Verificar si pertenece al objeto deseado -------------------

-- Verifica si un punto esta en el arco descrito
isInArc   :: Point -> Arc -> Bool
isInArc pt (Arc pc r al dir) = 
  if (abs((dist pt pc)-r) < eps && (validAng (pt-pc) inicio fin)) || (abs(dist pt pc) < 2*eps) then True else False
      where (inicio, fin) = (dir, dir-al)
            eps = (sqrt 1.9)*0.5
            validAng p a b =  (a <= ang p && ang p <= b) ||
                              (b <= ang p && ang p <= a) ||
                              (a + 360 <= (ang p) && (ang p) <= b + 360) ||
                              (b + 360 <= (ang p) && (ang p) <= a + 360) ||
                              (b + 720 <= (ang p) && (ang p) <= a + 720)
                              
            
-- Verifica si el punto esta en el segmento descrito
isInSeg :: Point -> Seg -> Bool
isInSeg p (Seg pos l dir) = (distPointSeg pos b p) < (sqrt 1.9)*0.5
    where 
          al = dir*pi/180
          b  = pos + (Point (l * (cos al)) (l * (sin al)))
          


---------------------- Funciones y estructuras para manejar ambos datos ---------------------------

-- Trazos de retina, o es un arco o es un segmento
type Line   = Either Arc Seg

-- Determina si un punto está en un trazo (arco o segmento)
isInLine :: (Int, Int) -> Line -> Bool
isInLine p' line = let p = fromPair p' in either (isInArc p) (isInSeg p) line

-- Determina si un punto pertence a al menos uno de los trazos dibujados
isOn :: (Int, Int) -> [Line] -> Bool
isOn p lines = elem True [isInLine p line | line <- lines]

-- dibuja todos los trazos simultaneamente
draw :: [Line] -> [String]
draw lines = reverse [[if (isOn (x, y) lines) then '1' else '0' | x<-[-500..500] ] | y<-[-500..500] ]


