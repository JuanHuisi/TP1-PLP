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
  Const x -> fConst x
  Rango x y -> fRango x y
  Suma a b -> fSuma a (recR a) b (recR b)
  Resta a b -> fResta a (recR a) b (recR b)
  Mult a b -> fMult a (recR a) b (recR b)
  Div a b -> fDiv a (recR a) b (recR b)

  where recR = recrExpr fConst fRango fSuma fResta fMult fDiv


foldExpr :: (Float -> a) -> (Float -> Float -> a) -> (a -> a -> a) -> (a -> a -> a) -> (a -> a -> a) -> (a -> a -> a) -> Expr -> a
foldExpr fConst fRango fSuma fResta fMult fDiv fExpr = case fExpr of
  Const x -> fConst x
  Rango x y -> fRango x y
  Suma a b -> fSuma (rec a) (rec b)
  Resta a b -> fResta (rec a) (rec b)
  Mult a b -> fMult (rec a) (rec b)
  Div a b -> fDiv (rec a) (rec b)

  where rec = foldExpr fConst fRango fSuma fResta fMult fDiv

-- acá se utilizo let para crear variables intermedias. 
-- Se evalua cada parte de la expresión que tenemos y la vamos guardando en el generador.
-- Lo hacemos para ambos elementos y para la operación final. 
eval :: Expr -> G Float
eval = foldExpr
        (,)                 -- Caso Const x
        (\d u -> dameUno (d, u))         -- Caso Rango x y


        {-  Para estos casos, la idea es que vamos a tener Suma x y, y además g será nuestro gen.
            Primero evaluamos x g para obtener (a, g1), luego y g1 para (b, g2),
            y finalmente combinamos con la operación y devolvemos g2. -}
        (evalBOp (+))  -- Caso suma 
        (evalBOp (-))  -- Caso resta
        (evalBOp (*))  -- Caso mult
        (evalBOp (/))  -- Caso div

  where
    evalBOp operacion x y g = let (a, g1) = x g
                                  (b, g2) = y g1
                              in (operacion a b, g2)



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
mostrar = recrExpr
            show    -- Const
            (\l u -> show l ++ "~" ++ show u)   -- Rango

            (toString [CEResta, CEMult, CEDiv] " + ") -- Suma

            (toString [CESuma, CEResta] " - ") -- Resta

            (toString [CESuma, CEResta, CEDiv] " * ") -- Mult

            (toString [CESuma, CEResta] " / ") -- Div

          where
            -- si el constructor del Expr esta en la lista, pone parentesis
            lleva_parent :: [ConstructorExpr] -> Expr -> String -> String
            lleva_parent cs e s = maybeParen (constructor e `elem` cs) s

            toString :: [ConstructorExpr] -> String -> Expr -> String -> Expr -> String -> String
            toString ce str exprA strA exprB strB = (lleva_parent ce exprA strA) ++ str ++ (lleva_parent ce exprB strB)


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
