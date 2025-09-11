-- | Un `Histograma` es una estructura de datos que permite contar cuántos valores hay en cada rango.
-- @vacio n (a, b)@ devuelve un histograma vacío con n+2 casilleros:
--
-- * @(-inf, a)@
-- * @[a, a + tamIntervalo)@
-- * @[a + tamIntervalo, a + 2*tamIntervalo)@
-- * ...
-- * @[b - tamIntervalo, b)@
-- * @[b, +inf)@
--
-- `vacio`, `agregar` e `histograma` se usan para construir un histograma.
module Histograma
  ( Histograma, -- No se exportan los constructores
    vacio,
    agregar,
    histograma,
    Casillero (..),
    casMinimo,
    casMaximo,
    casCantidad,
    casPorcentaje,
    casilleros,
  )
where

import Util
import Data.List (zipWith4)
data Histograma = Histograma Float Float [Int]
  deriving (Show, Eq)

-- | Inicializa un histograma vacío con @n@ casilleros para representar
-- valores en el rango y 2 casilleros adicionales para los valores fuera del rango.
-- Require que @l < u@ y @n >= 1@.
vacio :: Int -> (Float, Float) -> Histograma
vacio n (l, u) = (Histograma l u ([0 | x <- [0..n+1]]))

-- | Agrega un valor al histograma.
-- Pido perdón por el abuso de let pero realmente es muy útil. 
-- La correciones fueron varias. La primera es que utilizabamos el primer índice del array incorrectamente.
-- Recuerden que el primer casillero es el de -inf, y el segundo es el primero del rango. Utilizabamos el primero como el primero del primer rango, mal. Lo mismo al final.
-- Lo otro que estaba mal es que si se iba de rango, por ejemplo len xs = 5 y teníamos algo como 8, no se agregaba(mal).
--Con esto debería funcionar todo. Cualquier cosa me avisan.
agregar :: Float -> Histograma -> Histograma
agregar x (Histograma l u xs) =
  let n = length xs - 2  -- número de intervalos internos. Recordemos que los rangos van desde l hasta l+u, luego lu hasta l +2u...
      tamIntervalo = (u - l) / fromIntegral n
      indice
        | x < l     = 0
        | otherwise =
            let k = floor ((x - l) / tamIntervalo) + 1
            in if k > n then n + 1 else k --Para el caso en que se pase del rango superior
  in Histograma l u (actualizarElem indice (+1) xs)
-- Histograma (15 20 vacio) -> (-inf, 15) [15, 35) [35, 55) [55, 75) [75, +inf)]]]

-- | Arma un histograma a partir de una lista de números reales con la cantidad de casilleros y rango indicados.
histograma :: Int -> (Float, Float) -> [Float] -> Histograma
histograma n r = foldr (\x acc -> agregar x acc) (vacio n r)


-- | Un `Casillero` representa un casillero del histograma con sus límites, cantidad y porcentaje.
-- Invariante: Sea @Casillero m1 m2 c p@ entonces @m1 < m2@, @c >= 0@, @0 <= p <= 100@
data Casillero = Casillero Float Float Int Float
  deriving (Show, Eq)

-- | Mínimo valor del casillero (el límite inferior puede ser @-inf@)
casMinimo :: Casillero -> Float
casMinimo (Casillero m _ _ _) = m

-- | Máximo valor del casillero (el límite superior puede ser @+inf@)
casMaximo :: Casillero -> Float
casMaximo (Casillero _ m _ _) = m

-- | Cantidad de valores en el casillero. Es un entero @>= 0@.
casCantidad :: Casillero -> Int
casCantidad (Casillero _ _ c _) = c

-- | Porcentaje de valores en el casillero respecto al total de valores en el histograma. Va de 0 a 100.
casPorcentaje :: Casillero -> Float
casPorcentaje (Casillero _ _ _ p) = p

-- | Dado un histograma, devuelve la lista de casilleros con sus límites, cantidad y porcentaje.
casilleros :: Histograma -> [Casillero]
casilleros h = zipWith4 Casillero
                      (auxCasMin h)
                      (auxCasMax h)
                      (auxCascant h)
                      (auxCasPor h)

auxCasMin :: Histograma -> [Float]
auxCasMin (Histograma l u xs) =  [infinitoNegativo] ++ [(l + fromIntegral x * u) / ( fromIntegral (length xs) - 2) | x <- [0..(length xs - 2)]]

auxCasMax :: Histograma -> [Float]
auxCasMax (Histograma l u xs) =  [(l + fromIntegral x * u) / ( fromIntegral (length xs) - 2) | x <- [0..(length xs - 2)]] ++ [infinitoPositivo]

auxCascant :: Histograma -> [Int]
auxCascant (Histograma l u xs) = xs

auxCasPor :: Histograma -> [Float] --Chicos recuerden que tiene que dar 0.0, no NaN. Utilizo Let-in para poder chequear
--si el valor actual suma 0(NaN) y en ese caso devolver 0.0
auxCasPor (Histograma l u xs) = foldr (\x acc -> 
        let total = sum xs
            val = if total == 0 then 0 else (fromIntegral x / fromIntegral total) * 100
        in val : acc
      ) [] xs
