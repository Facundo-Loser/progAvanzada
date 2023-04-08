--Práctica 3:

--Ejercicio 1:
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = if x <= y then x:y:(merge xs ys) else y:x:(merge xs ys)

--Ejercicio 2:

--SelectionSort:
elimElem :: (Eq a) => a -> [a] -> [a]
elimElem n [] = []
elimElem n (x:xs) = if n == x then xs else x:(elimElem n xs)

selectionSort :: (Ord a) => [a] -> [a]
selectionSort [] = []
selectionSort xs = (minimun xs):(selectionSort (elimElem (minimum xs) xs))

--BubbleSort:
init' :: [a] -> [a]
init' [] = []
init' [x] = []
init' (x:xs) = x:(init' xs)

ult' :: [a] -> a 
ult' [x] -> x 
ult' (x:xs) = ult' xs

burbujeo :: (Ord a) => [a] -> [a]
burbujeo [] = []
burbujeo [x] = [x]
burbujeo (x:y:xs) = if x > y then y:(burbujeo (x:xs)) else x:(burbujeo (y:xs))

bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort [] = []
bubbleSort xs = bubbleSort (init' (burbujeo xs)) ++ [ult' (burbujeo xs)]

--Ejercicio 3:
potenciaDos :: Int -> Int
potenciaDos 0 = 1
potenciaDos n = 2 * (potenciaDos (n-1))

--Ejercicio 4:
toBinary :: Int -> [Int]
toBinary 0 = [0]
toBinary n = reverse (toBinaryHelper n)

toBinaryHelper :: Int -> [Int]
toBinaryHelper 0 = []
toBinaryHelper n = (n `mod` 2) : toBinaryHelper (n `div` 2)

--Otra versión:
numbin :: Int -> [Int]
numbin 0 = []
numbin 1 = [1]
numbin n = (numbin (div n 2)) ++ [mod n 2]

--Ejercicio 5:
ult' :: [a] -> a
ult' [x] = x
ult' (x:xs) = ult' xs

esParBinario :: [Int] -> Bool
esParBinario [] = False
esParBinario xs = (ult' xs) == 0 

--Función complementaria para ejercicio 6:
cantElem :: [a] -> Int
cantElem [] = 0
cantElem (_:xs) = 1 + cantElem xs

--Ejercicio 6:
distanciaH :: (Eq a) => [a] -> [a] -> Int
distanciaH [] [] = 0
distanciaH xs [] = 0
distanciaH [] ys = 0
distanciaH (x:xs) (y:ys) = if x == y then distanciaH xs ys else 1 + (distanciaH xs ys)

--Ejercicio 7:
cuadPerfecto :: Int -> Bool
cuadPerfecto n = [x | x <- [0..n], x*x == n] /= []

--Ejercicio 8:
cantApariciones :: (Eq a) => a -> [a] -> Int
cantApariciones n [] = 0
cantApariciones n (x:xs) = if n == x then 1 + (cantApariciones n xs) else cantApariciones n xs

repetidos :: (Eq a) => [a] -> a -> Int -> Bool
repetidos [] n z = False
repetidos xs n z = z == (cantApariciones n xs)

--Ejercicio 9:
nelem :: [a] -> a -> Int
nelem [] n = 0
nelem (x:_) 0 = x 
nelem (x:xs) n = nelem xs (n-1)

--Ejercicio 10:
darPosiciones :: (Eq a) => [a] -> a -> Int -> [Int]
darPosiciones [] n z = []
darPosiciones (x:xs) n z = if x == n then z:(darPosiciones xs n (z+1)) else darPosiciones xs n (z+1)

posicionesC :: (Eq a) => [a] -> a -> [Int]
posicionesC xs n = darPosiciones xs n 0

--Ejercicio 11:
compact :: (Eq a) => [a] -> [a]
compact [] = []
compact [x] = [x]
compact (x:y:xs) = if x == y then x:(compact xs) else x:(compact (y:xs))