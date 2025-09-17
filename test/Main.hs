module Main (main) where

import App
import Expr
import Expr.Parser
import GHC.Stack (HasCallStack)
import Generador
import Histograma
import Test.HUnit
import Util

main :: IO ()
main = runTestTTAndExit allTests

-- | Función auxiliar para marcar tests como pendientes a completar
completar :: (HasCallStack) => Test
completar = TestCase (assertFailure "COMPLETAR")

allTests :: Test
allTests =
  test
    [ "Ej 1 - Util.alinearDerecha" ~: testsAlinearDerecha,
      "Ej 2 - Util.actualizarElem" ~: testsActualizarElem,
      "Ej 3 - Histograma.vacio" ~: testsVacio,
      "Ej 4 - Histograma.agregar" ~: testsAgregar,
      "Ej 5 - Histograma.histograma" ~: testsHistograma,
      "Ej 6 - Histograma.casilleros" ~: testsCasilleros,
      "Ej 7 - Expr.recrExpr" ~: testsRecr,
      "Ej 7 - Expr.foldExpr" ~: testsFold,
      "Ej 8 - Expr.eval" ~: testsEval,
      "Ej 9 - Expr.armarHistograma" ~: testsArmarHistograma,
      "Ej 10 - Expr.evalHistograma" ~: testsEvalHistograma,
      "Ej 11 - Expr.mostrar" ~: testsMostrar,
      "Expr.Parser.parse" ~: testsParse,
      "App.mostrarFloat" ~: testsMostrarFloat,
      "App.mostrarHistograma" ~: testsMostrarHistograma
    ]

testsAlinearDerecha :: Test
testsAlinearDerecha =
  test
    [ alinearDerecha 6 "hola" ~?= "  hola",
      alinearDerecha 10 "incierticalc" ~?= "incierticalc",
      alinearDerecha 0 "abcd" ~?= "abcd",
      alinearDerecha 0 "   abcd" ~?= "   abcd",
      alinearDerecha (-2) " abcd" ~?= " abcd",
      alinearDerecha 4 "  tp" ~?= "  tp",
      alinearDerecha 3 "" ~?= "   "
    ]

testsActualizarElem :: Test
testsActualizarElem =
  test
    [ actualizarElem 0 (+ 10) [1, 2, 3] ~?= [11, 2, 3],
      actualizarElem 1 (+ 10) [1, 2, 3] ~?= [1, 12, 3],
      actualizarElem 3 (*2) [10, 20, 30, 40] ~?= [10, 20, 30, 80],
      actualizarElem 9 (*2) [10, 20, 30, 40] ~?= [10, 20, 30, 40],
      actualizarElem (-3) (*2) [10, 20, 30, 40] ~?= [10, 20, 30, 40],
      actualizarElem 0 (*2) [] ~?= []
    ]

testsVacio :: Test
testsVacio =
  test
    [ casilleros (vacio 1 (0, 10))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 10 0 0,
              Casillero 10 infinitoPositivo 0 0
            ],
      casilleros (vacio 3 (0, 6))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 2 0 0,
              Casillero 2 4 0 0,
              Casillero 4 6 0 0,
              Casillero 6 infinitoPositivo 0 0
            ],
      casilleros (vacio 2 (0, 5))
        ~?= [ Casillero infinitoNegativo 0 0 0,
              Casillero 0 2.5 0 0,
              Casillero 2.5 5 0 0,
              Casillero 5 infinitoPositivo 0 0]

      vacio 3 (0, 6) ~?= Histograma 0 2.0 [0,0,0,0,0]
      vacio 1 (0, 10) ~?= Histograma 0 10.0 [0,0,0]
      vacio 5 (300, 1000) ~?= Histograma 300.0 140.0 [0,0,0,0,0,0,0]
    ]

testsAgregar :: Test
testsAgregar =
  let h0 = vacio 3 (0, 6)
   in test
        [ casilleros (agregar 0 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 1 100, -- El 100% de los valores están acá
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar 2 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 0 0,
                  Casillero 2 4 1 100, -- El 100% de los valores están acá
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar (-1) h0)
            ~?= [ Casillero infinitoNegativo 0 1 100, -- El 100% de los valores están acá
                  Casillero 0 2 0 0,
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 0 0
                ],
          casilleros (agregar 5 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 0 0,
                  Casillero 2 4 0 0,
                  Casillero 4 6 1 100,
                  Casillero 6 infinitoPositivo 0 0],
          casilleros (agregar 10 h0)
            ~?= [ Casillero infinitoNegativo 0 0 0,
                  Casillero 0 2 0 0,
                  Casillero 2 4 0 0,
                  Casillero 4 6 0 0,
                  Casillero 6 infinitoPositivo 1 100
                ]
        ]

testsHistograma :: Test
testsHistograma =
  test
    [ histograma 4 (1, 5) [1, 2, 3] ~?= agregar 3 (agregar 2 (agregar 1 (vacio 4 (1, 5)))),
      histograma 2 (0, 4) [] ~?= vacio 2 (0, 4),
      histograma 3 (0,6) [-1,0,1,3,5,6,7] ~?= agregar 7 (agregar 6 (agregar 5 (agregar 3 (agregar 1 (agregar 0 (agregar (-1) (vacio 3 (0,6))))))))
    ]

testsCasilleros :: Test
testsCasilleros =
  test
    [ casilleros (vacio 3 (0, 6))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 0 0.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ],
      casilleros (agregar 2 (vacio 3 (0, 6)))
        ~?= [ Casillero infinitoNegativo 0.0 0 0.0,
              Casillero 0.0 2.0 0 0.0,
              Casillero 2.0 4.0 1 100.0,
              Casillero 4.0 6.0 0 0.0,
              Casillero 6.0 infinitoPositivo 0 0.0
            ]
    ]

testsRecr :: Test
testsRecr =
  test
    [ recrExpr id (\x y -> (x+y)/2)
                  (\_ x _ y -> x+y)
                  (\_ x _ y -> x-y)
                  (\_ x _ y -> x*y)
                  (\_ x _ y -> x/y)
                  (Const 2.5) ~?= 2.5

      recrExpr (const 1) (\_ _ -> 0) (\_ x _ y -> x+y) (\_ x _ y -> x+y) (\_ x _ y -> x+y) (\_ x _ y -> x+y) (Suma (Const 1.0) (Mult (Rango 0 1) (Const 2.0))) == 2

      recrExpr id (\x y -> (x+y)/2) (\_ x _ y -> x*y) (\_ x _ y -> x*y) (\_ x _ y -> x*y) (\_ x _ y -> x*y) (Mult (Const 2.0) (Mult (Rango 5.0 15.0) (Const 3.0))) == 60.0

      recrExpr id (\x y -> (x+y)/2) (\_ x _ y -> x+y) (\_ x _ y -> x-y) (\_ x _ y -> x*y) (\_ x _ y -> x/y) (Suma (Const 10.0) (Resta (Rango 5.0 15.0) (Const 2.0))) == 18.0 
      
    ]

testsFold :: Test
testsFold =
  test
    [ foldExpr id (\x y -> x+y/2) (+) (-) (*) (/) (Const 2.5) ~?= 2.5,
      foldExpr id (\x y -> (x+y)/2) (+) (-) (*) (/) (Rango 2 10) ~?= 6,
      foldExpr id (\x y -> x+y/2) (+) (-) (*) (/) (Suma (Const 2) (Const 5)) ~?= 7,
      foldExpr id (\x y -> x+y/2) (+) (-) (*) (/) (Mult (Const 2) (Const 5)) ~?= 10,
      foldExpr id (\x y -> x+y/2) (+) (-) (*) (/) (Div (Const 20) (Resta (Const 11) (Const 1))) ~?= 2
    ]

testsEval :: Test
testsEval =
  test
    [ 
     fst (eval (Const 2.5) genFijo) ~?= 2.5,
     fst (eval (Rango 1 5) (genNormalConSemilla 0)) ~?= 2.7980492,
     fst (eval (Suma (Const 2) (Const 5)) genFijo) ~?= 7.0,
     fst (eval (Suma (Suma (Const 2) (Const 5)) (Const 3)) genFijo) ~?= 10.0,
     fst (eval (Mult (Rango 1 5) (Const 2)) (genNormalConSemilla 0)) ~?= 5.5960984,
     fst (eval (Div (Const 20) (Resta (Const 11) (Const 1))) genFijo) ~?= 2.0,
      --Rango 3 5 -> 4. Rango 3 4 -> 3.5
     fst (eval (Resta (Rango 3 5) (Rango 3 4)) (genNormalConSemilla 5)) ~?= 1.001102,
     fst (eval (Div (Const 20) (Resta (Const 11) (Const 1)))) genFijo ~?= 2.0,
     fst (eval (Resta (Const 10) (Const 4)) genFijo) ~?= 6.0,

     fst (eval (Const 2.5) genFijo) ~?= 2.5
     fst (eval (Suma (Rango 1 5) (Const 1)) genFijo) ~?= 4.0,
     fst (eval (Suma (Rango 1 5) (Const 1)) (genNormalConSemilla 0)) ~?= 3.7980492,
      -- el primer rango evalua a 2.7980492 y el segundo a 3.1250308
     fst (eval (Suma (Rango 1 5) (Rango 1 5)) (genNormalConSemilla 0)) ~?= 5.92308,
     fst (eval (Rango 1 5) (genNormalConSemilla 0)) ~?= 2.7980492,
     fst (eval (Div (Const 18) (Mult (Rango 1 5) (Const 2))) genFijo) ~?= 3.0
    ]

testsArmarHistograma :: Test
testsArmarHistograma =
  test
    [
      armarHistograma 11 10 (eval (Suma (Rango 1 5) (Rango 100 105))) genFijo ~?=(Histograma 104.5 0.18181819 [0,0,0,0,0,0,10,0,0,0,0,0,0],<Gen>)

      armarHistograma 15 20 (eval(Resta (Rango 15 30) (Rango 2 2) )) genFijo ~?=(Histograma 19.5 0.13333334 [0,0,0,0,0,0,0,0,20,0,0,0,0,0,0,0,0],<Gen>)

      armarHistograma 5 5 (eval (Mult(Rango 2 2) (Rango 3 3))) (genNormalConSemilla 3)~?=(Histograma 5.0 0.4 [0,0,0,5,0,0,0],<Gen>)

      armarHistograma 9 9 (eval(Div (Rango 1 3) (Rango 5 8))) (genNormalConSemilla 9)~?=(Histograma 0.115138665 3.1622965e-2 [0,0,2,1,2,0,0,3,0,1,0],<Gen>)
    ]

testsEvalHistograma :: Test
testsEvalHistograma =
  test
    [
      --Test catedra
      evalHistograma 11 10 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0) ~?= (Histograma 102.005486 0.6733038 [1,0,0,0,1,3,1,2,0,0,1,1,0],<Gen>)
      evalHistograma 11 10000 (Suma (Rango 1 5) (Rango 100 105)) (genNormalConSemilla 0) ~?= (Histograma 102.273895 0.5878462 [239,288,522,810,1110,1389,1394,1295,1076,793,520,310,254],<Gen>) 
      --Test propios
      evalHistograma 5 1 (Rango 1 5) genFijo ~?= (Histograma 2.0 0,4 [0,0,0,1,0,0,0],<Gen>)
      evalHistograma 10 5 (Suma (Rango 1 5) (Const 2)) genFijo ~?= (Histograma 4.0 0.2 [0,0,0,0,0,0,5,0,0,0,0,0],<Gen>)
      evalHistograma 10 5 (Suma (Rango 1 5) (Const 2)) (genNormalConSemilla 0) ~?= (Histograma 2.7765357 0.5019105 [0,0,1,0,0,2,1,0,0,0,1,0],<Gen>)
    ]

testsParse :: Test
testsParse =
  test
    [ parse "1" ~?= Const 1.0,
      parse "-1.7 ~ -0.5" ~?= Rango (-1.7) (-0.5),
      parse "1+2" ~?= Suma (Const 1.0) (Const 2.0),
      parse "1 + 2" ~?= Suma (Const 1.0) (Const 2.0),
      parse "1 + 2 * 3" ~?= Suma (Const 1.0) (Mult (Const 2.0) (Const 3.0)),
      parse "1 + 2 + 3" ~?= Suma (Suma (Const 1.0) (Const 2.0)) (Const 3.0),
      parse "1 + (2 + 3)" ~?= Suma (Const 1.0) (Suma (Const 2.0) (Const 3.0)),
      parse "1 + 2 ~ 3 + 4" ~?= Suma (Suma (Const 1.0) (Rango 2.0 3.0)) (Const 4.0),
      parse "1 - 2 - 3 - 4" ~?= Resta (Resta (Resta (Const 1.0) (Const 2.0)) (Const 3.0)) (Const 4.0),
      parse "(((1 - 2) - 3) - 4)" ~?= Resta (Resta (Resta (Const 1.0) (Const 2.0)) (Const 3.0)) (Const 4.0),
      parse "1 " ~?= Const 1.0,
      parse "   1    " ~?= Const 1.0
    ]

testsMostrar :: Test
testsMostrar =
  test
    [ mostrar (Div (Suma (Rango 1 5) (Mult (Const 3) (Rango 100 105))) (Const 2))
        ~?= "(1.0~5.0 + (3.0 * 100.0~105.0)) / 2.0",
      mostrar (Suma (Suma (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Suma (Const 1) (Suma (Const 2) (Suma (Const 3) (Const 4))))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Suma (Suma (Const 1) (Const 2)) (Suma (Const 3) (Const 4)))
        ~?= "1.0 + 2.0 + 3.0 + 4.0",
      mostrar (Mult (Mult (Mult (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Mult (Const 1) (Mult (Const 2) (Mult (Const 3) (Const 4))))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Mult (Mult (Const 1) (Const 2)) (Mult (Const 3) (Const 4)))
        ~?= "1.0 * 2.0 * 3.0 * 4.0",
      mostrar (Resta (Resta (Const 1) (Const 2)) (Resta (Const 3) (Const 4)))
        ~?= "(1.0 - 2.0) - (3.0 - 4.0)",
      mostrar (Resta (Resta (Resta (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "((1.0 - 2.0) - 3.0) - 4.0",
      mostrar (Suma (Mult (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "((1.0 + 2.0) * 3.0) + 4.0",
      mostrar (Mult (Suma (Suma (Const 1) (Const 2)) (Const 3)) (Const 4))
        ~?= "(1.0 + 2.0 + 3.0) * 4.0"
    ]

testsMostrarFloat :: Test
testsMostrarFloat =
  test
    [ mostrarFloat 0.0 ~?= "0.00",
      mostrarFloat 1.0 ~?= "1.00",
      mostrarFloat (-1.0) ~?= "-1.00",
      -- Redondeo
      mostrarFloat 3.14159 ~?= "3.14",
      mostrarFloat 2.71828 ~?= "2.72",
      mostrarFloat 0.000001 ~?= "1.00e-6",
      mostrarFloat 100000 ~?= "100000.00",
      -- Infinitos
      mostrarFloat infinitoPositivo ~?= "+inf",
      mostrarFloat infinitoNegativo ~?= "-inf"
    ]

testsMostrarHistograma :: Test
testsMostrarHistograma =
  let h0 = vacio 3 (0, 6)
      h123 = agregar 1 (agregar 2 (agregar 3 h0))
   in test
        [ lines (mostrarHistograma h123)
            ~?= [ "6.00 - +inf |",
                  "4.00 - 6.00 |",
                  "2.00 - 4.00 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ 66.67%",
                  "0.00 - 2.00 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒",
                  "-inf - 0.00 |"
                ],
          lines (mostrarHistograma (agregar 1 (vacio 3 (0, 1000))))
            ~?= [ "  1000.00 - +inf |",
                  "666.67 - 1000.00 |",
                  " 333.33 - 666.67 |",
                  "   0.00 - 333.33 |▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒▒ 100.00%",
                  "     -inf - 0.00 |"
                ]
        ]
