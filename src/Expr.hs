module Expr
  ( Expr (..),
    recrExpr,
    foldExpr,
    eval,
    armarHistograma,
    evalHistograma,
    mostrar,
  )
where

import Generador
import Histograma

-- | Expresiones aritméticas con rangos
data Expr
  = Const Float
  | Rango Float Float
  | Suma Expr Expr
  | Resta Expr Expr
  | Mult Expr Expr
  | Div Expr Expr
  deriving (Show, Eq)

recrExpr :: (Float -> a) -> (Float -> Float -> a) -> (Expr -> a -> Expr -> a -> a) -> (Expr -> a -> Expr -> a -> a) -> (Expr -> a -> Expr -> a -> a) -> (Expr -> a -> Expr -> a -> a) -> Expr -> a
recrExpr fConst fRango fSuma fResta fMult fDiv fExpr = case fExpr of
  (Const x) -> fConst x
  (Rango x y) -> fRango x y
  (Suma a b) -> fSuma a (recR a) b (recR b)
  (Resta a b) -> fResta a (recR a) b (recR b)
  (Mult a b) -> fMult a (recR a) b (recR b)
  (Div a b) -> fDiv a (recR a) b (recR b)
  
  where recR = recrExpr fConst fRango fSuma fResta fMult fDiv


foldExpr :: (Float -> a) -> (Float -> Float -> a) -> (a -> a -> a) -> (a -> a -> a) -> (a -> a -> a) -> (a -> a -> a) -> Expr -> a 
foldExpr fConst fRango fSuma fResta fMult fDiv fExpr = case fExpr of
  (Const x) -> fConst x
  (Rango x y) -> fRango x y
  (Suma a b) -> fSuma (rec a) (rec b)
  (Resta a b) -> fResta (rec a) (rec b)
  (Mult a b) -> fMult (rec a) (rec b)
  (Div a b) -> fMult (rec a) (rec b)

  where rec = foldExpr fConst fRango fSuma fResta fMult fDiv

-- | Evaluar expresiones dado un generador de números aleatorios
--Definir la funci´on eval que permite obtener una posible evaluaci´on de una expresi´on dado
 --un generador de n´umeros aleatorios. No se permite recursi´on expl´ıcita.
 --El tipo deseado es eval :: Expr → Gen → (Float, Gen) para poder obtener el generador
 --resultante despu´es de generar algunos n´umeros aleatorios para evaluar los rangos. Puede escribirse
 --usando G a como eval :: Expr → G Float.
 --Se debe usar dameUno para determinar el valor de un rango.
eval :: Expr -> G Float
eval =
  foldExpr
    (\x g -> (x, g)) --Caso const x
    (\l u g -> dameUno (l, u) g) -- Caso Rango. Por enunciado tenemos que utilizar dameUno.
    (\fa fb g -> case fa g of
                    (x, g1) -> case fb g1 of
                                 (y, g2) -> (x + y, g2)) --Caso Suma. Es bastante extenso de explicar.
                                 -- La idea es que nosotros vamos a tener Suma fa fb, y el g va a ser nuestro gen.
                                 -- Entonces, primero evaluo fa g para modificar el generador, y luego con el generador modificado g1 calculo fb g1. 
                                 -- Esto lo guardo en g2, y luego devuelvo la suma con el generador g2. 
                                 -- Basicamente esto se repite con todos los demás casos. CHEQUEAR QUE PASA EN LA DIVISIÓN SI LE PASAMOS 0.
    (\fa fb g -> case fa g of
                    (x, g1) -> case fb g1 of 
                                 (y, g2) -> (x - y, g2))
    (\fa fb g -> case fa g of
                    (x, g1) -> case fb g1 of
                                 (y, g2) -> (x * y, g2))
    (\fa fb g -> case fa g of
                    (x, g1) -> case fb g1 of 
                                 (y, g2) -> (x / y, g2))

--Otra manera de hacer eval. La explicación es la misma que la de arriba, solamente que acá utilizo let para crear variables intermedias.
--El proceso es lo mismo, voy evaluando cada parte de la expresión que genemos y la vamos guardando en el generador.
--Lo hacemos para ambos elementos y para la operación final. 
--Usemos la que mas les guste. Recuerden que si usan esta, cambien el nombre de eval2 a eval.
eval2 :: Expr -> G Float
eval2 = foldExpr (\x g -> (x, g))
                 (\d u g -> dameUno (d, u) g)
                 (\x y g -> let (a, g1) = x g
                                (b, g2) = y g1 
                            in (a + b, g2))
                  (\x y g -> let (a, g1) = x g
                                 (b, g2) = y g1  
                              in (a - b, g2))
                    (\x y g -> let (a, g1) = x g
                                   (b, g2) = y g1
                              in (a * b, g2)) 
                    (\x y g -> let (a, g1) = x g
                                   (b, g2) = y g1
                              in (a / b, g2)) 

-- | @armarHistograma m n f g@ arma un histograma con @m@ casilleros
-- a partir del resultado de tomar @n@ muestras de @f@ usando el generador @g@.
armarHistograma :: Int -> Int -> G Float -> G Histograma
armarHistograma m n f g = (histograma m (rango95 (fst (muestra f n g))) 
                          (fst (muestra f n g)), snd(muestra f n g))

-- | @evalHistograma m n e g@ evalúa la expresión @e@ usando el generador @g@ @n@ veces
-- devuelve un histograma con @m@ casilleros y rango calculado con @rango95@ para abarcar el 95% de confianza de los valores.
-- @n@ debe ser mayor que 0.
evalHistograma :: Int -> Int -> Expr -> G Histograma
evalHistograma m n expr = error "COMPLETAR EJERCICIO 10"

-- Podemos armar histogramas que muestren las n evaluaciones en m casilleros.
-- >>> evalHistograma 11 10 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.005486 0.6733038 [1,0,0,0,1,3,1,2,0,0,1,1,0],<Gen>)

-- >>> evalHistograma 11 10000 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.273895 0.5878462 [239,288,522,810,1110,1389,1394,1295,1076,793,520,310,254],<Gen>)

-- | Mostrar las expresiones, pero evitando algunos paréntesis innecesarios.
-- En particular queremos evitar paréntesis en sumas y productos anidados.
mostrar :: Expr -> String
mostrar = error "COMPLETAR EJERCICIO 11"

data ConstructorExpr = CEConst | CERango | CESuma | CEResta | CEMult | CEDiv
  deriving (Show, Eq)

-- | Indica qué constructor fue usado para crear la expresión.
constructor :: Expr -> ConstructorExpr
constructor (Const _) = CEConst
constructor (Rango _ _) = CERango
constructor (Suma _ _) = CESuma
constructor (Resta _ _) = CEResta
constructor (Mult _ _) = CEMult
constructor (Div _ _) = CEDiv

-- | Agrega paréntesis antes y después del string si el Bool es True.
maybeParen :: Bool -> String -> String
maybeParen True s = "(" ++ s ++ ")"
maybeParen False s = s
