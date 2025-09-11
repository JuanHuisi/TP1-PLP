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
  (Div a b) -> fDiv (rec a) (rec b)

  where rec = foldExpr fConst fRango fSuma fResta fMult fDiv


--COMENTARIO IMPORTANTE: Tanto eval como eval2 funcionan perfectamente. Pueden comprobarlo utilizando
--eval (Cualquier operación (Rango a a) (Rango b b)) (genNormalConSemilla s) y ver que siempre les da a+b por la definición de que si l==u devuelve l.


--Otra manera de hacer eval. La explicación es la misma que la de arriba, solamente que acá utilizo let para crear variables intermedias.
--El proceso es lo mismo, voy evaluando cada parte de la expresión que genemos y la vamos guardando en el generador.
--Lo hacemos para ambos elementos y para la operación final. 
--Usemos la que mas les guste.
eval :: Expr -> G Float
eval = foldExpr
        (\x g -> (x, g)) -- Caso const x

        (\d u g -> dameUno (d, u) g) -- Caso Rango d u

        {-  Para estos casos, la idea es que nosotros vamos a tener Suma x y  ; y el g va a ser nuestro gen.
        Entonces, primero evalua x g para modificar el generador, y luego con el generador modificado g1 calculo y g1. 
        Esto lo guardo en g2, y luego devuelvo la suma con el generador g2. 
        Basicamente esto se repite con todos los dem    -}
        (\x y g -> let (a, g1) = x g  -- Caso suma 
                       (b, g2) = y g1
                   in (a + b, g2))

        (\x y g -> let (a, g1) = x g  -- Caso resta 
                       (b, g2) = y g1
                   in (a - b, g2))

        (\x y g -> let (a, g1) = x g  -- Caso mult 
                       (b, g2) = y g1
                   in (a * b, g2))

        (\x y g -> let (a, g1) = x g  -- Caso div
                       (b, g2) = y g1
                   in (a / b, g2))

-- | @armarHistograma m n f g@ arma un histograma con @m@ casilleros
-- a partir del resultado de tomar @n@ muestras de @f@ usando el generador @g@.
armarHistograma :: Int -> Int -> G Float -> G Histograma
armarHistograma m n f g = let (xs, g') = muestra f n g
                              r = rango95 xs
                          in (histograma m r xs, g')

evalHistograma :: Int -> Int -> Expr -> G Histograma
evalHistograma m n expr = armarHistograma m n (eval expr)

-- Podemos armar histogramas que muestren las n evaluaciones en m casilleros.
-- >>> evalHistograma 11 10 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.005486 0.6733038 [1,0,0,0,1,3,1,2,0,0,1,1,0],<Gen>)

-- >>> evalHistograma 11 10000 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0)
-- (Histograma 102.273895 0.5878462 [239,288,522,810,1110,1389,1394,1295,1076,793,520,310,254],<Gen>)

-- | Mostrar las expresiones, pero evitando algunos paréntesis innecesarios.
-- En particular queremos evitar paréntesis en sumas y productos anidados.
--ghci> mostrar (Suma (Suma (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
-- "1.0 + 2.0 + 3.0 + 4.0"
--ghci> mostrar (Div (Suma (Rango 1 5) (Mult (Const 3) (Rango 100 105))) (Const 2))
-- "(1.0∼5.0 + (3.0 * 100.0∼105.0)) / 2.0"


-- uso recr porque necesito las expresiones originales, para saber su constructor y decidir en funcion de el si lleva parentesis.
mostrar :: Expr -> String
mostrar = recrExpr (\x -> show x) -- Const
                   (\l u -> show l ++ "~" ++ show u) -- Rango

                   (\exprA strA exprB strB ->
                     let parentesisA = case constructor exprA of
                           CEResta -> True
                           CEMult -> True
                           CEDiv -> True
                           _ -> False
                         parentesisB = case constructor exprB of
                          CEResta -> True
                          CEMult -> True
                          CEDiv -> True
                          _ -> False
                     in maybeParen parentesisA strA ++ " + " ++ maybeParen parentesisB strB) -- Suma
                   
                   (\exprA strA exprB strB -> 
                     let parentesisA = case constructor exprA of
                           CEResta -> True
                           CESuma -> True
                           _ -> False
                         parentesisB = case constructor exprB of
                           CEResta -> True
                           CESuma -> True
                           _ -> False
                     in maybeParen parentesisA strA ++ " - " ++ maybeParen parentesisB strB) -- Resta

                   (\exprA strA exprB strB -> 
                     let parentesisA = case constructor exprA of
                           CEResta -> True
                           CESuma -> True
                           CEDiv -> True
                           _ -> False
                         parentesisB = case constructor exprB of
                          CEResta -> True
                          CESuma -> True
                          CEDiv -> True
                          _ -> False
                     in maybeParen parentesisA strA ++ " * " ++ maybeParen parentesisB strB) -- Multiplicación.

                   (\exprA strA exprB strB -> 
                     let parentesisA = case constructor exprA of
                           CEResta -> True
                           CESuma -> True
                           _ -> False
                     in maybeParen parentesisA strA ++ " / " ++ strB) -- División.


-- parentesis si es suma con algo que no es suma 
-- parentesis si es mult con algo que no es mult 
-- no parentesis si es const  
-- no parentesis si rango 
-- parentesis si izq en division es suma o resta
-- parentesis en resta si son sumas o restas 



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
