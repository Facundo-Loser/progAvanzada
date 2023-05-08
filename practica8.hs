--Ejercicio 1:
nand :: Bool -> Bool -> Bool
nand True True = False
nand True False = True
nand False True = True
nand False False = True

--Ejercicio 2:
maj :: Bool -> Bool -> Bool -> Bool 
maj  n m z  = (n && m) || (n && z) || (m && z)

--Ejercicio 3:

--Primer cuantificador:
existe :: [Int] -> [a] -> (Int -> [a] -> Bool) -> Bool
existe [] [] _ = False
existe _ [] _  = False
existe [] _ _  = False
existe xs ys f = or [f x ys | x <- xs]

--Segundo Cuantificador:
paraTodo :: [Int] -> [a] -> (Int -> [a] -> Bool)-> Bool
paraTodo [] [] _ = False
paraTodo _ [] _  = False
paraTodo [] _ _  = False
paraTodo xs ys f = and [f x ys | x <- xs]

--Funciones para probar los cuantificadores:
even' :: (Integral a) => Int -> [a] -> Bool
even' _ [] = False
even' n xs = even (xs !! n)

odd' :: (Integral a) => Int -> [a] -> Bool
odd' _ [] = False
odd' n xs = odd (xs !! n)


--Ejercicio 4:

--Cuantificador Sumatoria:
sumatoria :: [Int] -> [Int] -> Int
sumatoria [] [] = maxBound :: Int
sumatoria [] _  = maxBound :: Int
sumatoria _ []  = maxBound :: Int
sumatoria xs ys = sum [ys !! x | x <- xs]

--Cuantificador Productoria:
productoria :: [Int] -> [Int] -> Int
productoria [] [] = maxBound :: Int
productoria [] _  = maxBound :: Int
productoria _ []  = maxBound :: Int
productoria xs ys = product [ys !! x | x <- xs]

--Cuantificador Contatoria:
contatoria :: [Int] -> [a] -> (Int -> [a] -> Bool) -> Int
contatoria [] [] _ = maxBound :: Int
contatoria [] _  _ = maxBound :: Int
contatoria _ [] _  = maxBound :: Int
contatoria xs ys f = sum [1 | x <- xs, f x ys]