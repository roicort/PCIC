-- Examen 2 Haskell 2025-1
import Prelude hiding (foldl', concat)

-- I. Considerando las siguientes definiciones de funciones:

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ e []     = e
foldr' g e (x:xs) = g x (foldr' g e xs)

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ e []     = e
foldl' g e (x:xs) = foldl' g (g e x) xs

-- II. Definir la función todosImpares :: [Int] -> Bool 
-- Que verifica si todos los elementos de una lista xs son impares.

-- Ejemplo:
-- todosImpares [1, 3, 5] == True
-- todosImpares [1, 3, 5, 6] == False

todosImpares :: [Int] -> Bool
todosImpares xs = all odd xs

todosImpares2 :: [Int] -> Bool
todosImpares2 xs = foldr' (\x y -> odd x && y) True xs

-- III. Explica el significado de las siguientes expresiones

-- Función que implementa la primera expresión: Eq a => a -> a -> Bool
-- Compara si dos valores son iguales
esIgual :: Eq a => a -> a -> Bool
esIgual x y = x == y

-- Función que implementa la segunda expresión: Eq a => a -> [a] -> Bool
-- Verifica si un elemento está en una lista
esElemento :: Eq a => a -> [a] -> Bool
esElemento x xs = x `elem` xs

-- IV. La función concat tiene la siguiente definición:

concat [] = []
concat (xs:xss) = xs ++ concat xss

concatFoldr :: [[a]] -> [a]
concatFoldr = foldr (++) []

-- Main

main :: IO ()
main = do

    -- Ejercicio I

    putStrLn ""
    putStrLn "Ejercicio I"
    putStrLn ""
    
    let lista = [81, 27, 9, 3]
    
    -- a) foldl (-) 1 [81, 27, 9, 3]
    print $ foldl' (-) 1 lista

    -- b) foldr (-) 1 [81, 27, 9, 3]
    print $ foldr' (-) 1 lista

    -- c) foldr (/) 1 [81, 27, 9, 3]
    print $ foldr' (/) 1 lista

    -- d) foldl (/) 81 [9, 3, 3, 1]
    print $ foldl' (/) 81 [9, 3, 3, 1]

    -- Ejercicio II

    putStrLn ""
    putStrLn "Ejercicio II"
    putStrLn ""

    -- a) todosImpares [1, 3, 5]
    print $ todosImpares [1, 3, 5]

    -- b) todosImpares [1, 3, 5, 6]
    print $ todosImpares [1, 3, 5, 6]

    -- Ejericio III

    putStrLn ""
    putStrLn "Ejercicio III"
    putStrLn ""

    putStrLn "Primera expresión: Eq a => a -> a -> Bool"
    print $ esIgual 3 3         -- True: 3 es igual a 3
    print $ esIgual 3 4         -- False: 3 no es igual a 4
    print $ esIgual "hola" "hola" -- True: las cadenas son iguales
    print $ esIgual "hola" "mundo" -- False: las cadenas son diferentes

    -- Ejemplo 2: Segunda expresión (Eq a => a -> [a] -> Bool)
    putStrLn "\nSegunda expresión: Eq a => a -> [a] -> Bool"
    print $ esElemento 3 [1, 2, 3, 4]       -- True: 3 está en la lista
    print $ esElemento 5 [1, 2, 3, 4]       -- False: 5 no está en la lista
    print $ esElemento 'a' "hola mundo"     -- True: 'a' está en la cadena
    print $ esElemento 'z' "hola mundo"     -- False: 'z' no está en la cadena

    -- Ejercicio IV

    putStrLn ""
    putStrLn "Ejercicio IV"
    putStrLn ""

    let listas = [[1, 2, 3], [4, 5, 6], [7, 8]]

    print $ concat listas       -- Resultado: [1,2,3,4,5,6,7,8]
    print $ concatFoldr listas  -- Resultado: [1,2,3,4,5,6,7,8]

    -- Ejercicio V

    putStrLn ""
    putStrLn "Ejercicio V"
    putStrLn ""

    let f = (+1)
    let g = (*2)
    let h = f . g  -- h(x) = f(g(x)) = (x * 2) + 1

    print $ h 3 -- Resultado: 7
