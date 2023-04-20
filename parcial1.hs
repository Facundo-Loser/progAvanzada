--Parcial 2021:

data BinTree a = Nil | Node (BinTree a) a (BinTree a)

--let aBinTree = (Node (Node Nil 4 Nil) 2 (Node Nil 6 Nil))

--Ejercicio 1: (Suma todos los elementos de un arbol de enteros) 
sumTree :: BinTree Int -> Int
sumTree Nil = 0
sumTree (Node hi r hd) = r+(sumTree hi)+(sumTree hd)

--Ejercicio 3:
compact :: [Int] -> [Int]
compact [] = []
compact [x] = [x]
compact (x:y:xs)
        | x == y = y:(compact xs)
        | otherwise = x:(compact (y:xs))

compact2 :: [Int] -> [Int]
compact2 [] = []
compact2 [x] = [x]
compact2 (x:y:xs)
        | x == y = compact2 (y:xs)
        | otherwise = x:(compact2 (y:xs))