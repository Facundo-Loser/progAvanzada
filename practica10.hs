--Práctica 10:
--Implementación de todas las funciones obtenidas con derivación

-- 3)
f :: (Eq a) => [a] -> Bool
f [] = True
f (x:xs) = g x xs

g :: (Eq a) => a -> [a] -> Bool
g _ [] = True
g z (x:xs) = (z == x) && (g z xs)

-- 4) 
f1 :: [Int] -> Int
f1 [] = 1
f1 (x:xs) = x * f1 xs

-- 5) 
f2 :: (Ord a) => [a] -> Bool
f2 [] = True
f2 [x] = True
f2 (y:x:xs) = (y <= x) && f2 (x:xs)

-- 6) 
f3 :: [Int] -> Int
f3 [] = maxBound :: Int
f3 (x:xs) = min x (f3 xs)

-- 7) (chequear)
f4 :: [Int] -> Bool
f4 [] = False
f4 (x:xs) = g4 0 (x:xs)

g4 :: Int -> [Int] -> Bool
g4 _ [] = False
g4 z (x:xs) = (x == z + sum xs) || g4 z xs

-- 10) (chequear)
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

contatoria :: [Int] -> Int
contatoria [] = 0
contatoria (x:xs) = 1 + contatoria xs

prom :: [Int] -> (Int, Int)
prom [] = (0, 0)
prom [x] = (x , 1)
prom (x:xs) = (x+a, 1+b)
        where (a,b) = prom xs

promFinal :: [Int] -> Int
promFinal xs = div (fst prom xs) (snd prom xs) 