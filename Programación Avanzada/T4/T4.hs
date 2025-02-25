-- Cuarta Tarea.
-- Programación en Haskell

-- Autor: Rodrigo S. Cortez Madrigal
-- Fecha: 25 de octubre de 2024
-- Curso: Programación Avanzada

-- Programa en Haskell las siguientes funciones:

import Prelude hiding (until)
import Data.Complex
import Text.Printf (printf)

-- a) Calcular el área de un círculo.

areaCirculo :: Float -> Float
areaCirculo r = pi * r^2

-- b) Define una función que indique que dos listas son iguales aunque los elementos aparezcan en
-- distinto orden (conjuntos en Haskell).

iguales :: Eq a => [a] -> [a] -> Bool
iguales xs ys = (length xs == length ys) && (all (\x -> elem x ys) xs)

-- c) La función de Ackermann.

ackermann :: Int -> Int -> Int
ackermann 0 n = n + 1
ackermann m 0 = ackermann (m - 1) 1
ackermann m n = ackermann (m - 1) (ackermann m (n - 1))

-- d) Calcular las raíces de una ecuación de segundo grado, ya sean reales o imaginarias.

raices :: Float -> Float -> Float -> String
raices a b c
    | a == 0 = "El coeficiente 'a' no puede ser cero."
    | disc > 0 = printf "Dos raíces reales: x1 = %.2f, x2 = %.2f" x1 x2
    | disc == 0 = printf "Una raíz real: x = %.2f" x1
    | otherwise = printf "Dos raíces complejas: x1 = %.2f + %.2fi, x2 = %.2f - %.2fi" realPart imaginaryPart realPart imaginaryPart
    where
        disc = b^2 - 4 * a * c
        x1 = (-b + sqrt disc) / (2 * a)
        x2 = (-b - sqrt disc) / (2 * a)
        realPart = -b / (2 * a)
        imaginaryPart = sqrt (abs disc) / (2 * a)

-- e) Escribe 3 ejemplos de uso de la función until y explica el resultado obtenido.

-- until p f x aplica f a x hasta que el predicado p sea verdadero.

until :: (a -> Bool) -> (a -> a) -> a -> a
until p f x = if p x then x else until p f (f x)

-- Lo que hace la función until es aplicar la función f a x hasta que se cumpla el predicado p.

-- Ejemplo 1:
-- until (> 100) (* 2) 1: La función multiplica x por 2 hasta que x sea mayor a 100.
-- Resultado: 128

-- Ejemplo 2:
-- until (>= 100) (+ 10) 1: La función suma 10 a x hasta que x sea mayor o igual a 100.
-- Resultado: 101

-- Ejemplo 3:
-- until (== 100) (+ 1) 1: La función suma 1 a x hasta que x sea igual a 100.
-- Resultado: 100

-- Pruebas 

-- Llamar funciones e imprimir resultados

main :: IO ()
main = do
    putStrLn ""
    putStrLn "### Tarea 4 - Programación Avanzada ###"
    putStrLn ""
    printf "Area de un circulo de radio 5: %.2f\n" (areaCirculo 5)
    printf "Area de un circulo de radio 10: %.2f\n" (areaCirculo 10)
    putStrLn ""
    printf "Listas [1,2,3] [3,2,1] iguales: %s\n" (show (iguales [1,2,3] [3,2,1]))
    printf "Listas [1,2,3] [1,4,3] iguales: %s\n" (show (iguales [1,2,3] [1,4,3]))
    putStrLn ""
    printf "Ackermann 3 3: %d\n" (ackermann 3 3)
    printf "Ackermann 3 4: %d\n" (ackermann 3 4)
    putStrLn ""
    printf "Raices a=1 b=2 c=1: %s\n" (raices 1 2 1)
    printf "Raices a=1 b=-3 c=2: %s\n" (raices 1 (-3) 2)
    printf "Raices a=1 b=2 c=5: %s\n" (raices 1 2 5)
    putStrLn ""
    printf "Until (> 100) (* 2) 1: %s\n" (show (until (> 100) (* 2) 1))
    printf "Until (>= 100) (+ 10) 1: %s\n" (show (until (>= 100) (+ 10) 1))
    printf "Until (== 100) (+ 1) 1: %s\n" (show (until (== 100) (+ 1) 1))
    putStrLn ""