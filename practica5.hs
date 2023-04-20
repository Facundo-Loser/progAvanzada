--Ejercicio 1:
linf = 1:linf 

--Ejercicio 2:
listNat :: Int -> [Int]
listNat n = n:(listNat (n + 1))

--Ejercicio 3:
pListNat :: Int -> [Int]
pListNat 0 = []
pListNat n = pListNat(n-1) ++ [n]

primNat :: Int -> [Int]
primNat n = [0..n]

--Ejercicio 4:
primCinco :: [Int] -> [Int]
primCinco xs = aux xs 1

aux :: [Int] -> Int -> [Int]
aux (x:xs) 5 = [x]
aux (x:xs) n = x:(aux xs (n+1))

--Ejercicio 5:
cuadList :: [Int] -> [Int]
cuadList [] = []
cuadList xs = map (^2) xs

--Ejercicio 6:
divisores :: Int -> [Int]
divisores n = auxDiv n 1

auxDiv :: Int -> Int -> [Int]
auxDiv x n 
    | x == n = [n, (-n)]
    | n < x = if (mod x n) == 0 then n:(-n):(auxDiv x (n+1)) else auxDiv x (n+1)

--Ejercicio 7:
--Función auxiliar:
primo :: Int -> Bool
primo 1 = False
primo n = and [mod n i /= 0 | i <- [2..(n-1)]]

listPrimos :: [Int] -> [Int]
listPrimos [] = []
listPrimos (x:xs) = if primo x then x:(listPrimos xs) else listPrimos xs

--Solucción correcta:
soloPrimos :: [Int] -> [Int]
soloPrimos [] = []
soloPrimos xs = filter (primo) xs

--Ejercicio 8:
--Función auxiliar:
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + (sumatoria xs)

sumCuad :: [Int] -> Int
sumCuad xs = sumatoria(cuadList xs)

--Solución correcta:
cuads' :: [Int] -> [Int]
cuads' [] = []
cuads' xs = map (^2) xs

sumCuads' :: [Int] -> Int
sumCuads' xs = foldr (+) 0 (cuads' xs)

--Ejercicio 9:
listSucc :: [Int] -> [Int]
listSucc [] = []
listSucc (x:xs) = (succ x):(listSucc xs)

--Solución correcta:
sucesores' :: [Int] -> [Int]
sucesores' [] = []
sucesores' xs = map (succ) xs

--Ejercicio 10:
sumList :: [Int] -> Int
sumList [] = 0
sumList xs = foldr (+) 0 xs

--Ejercicio 11:
fact :: [Int] -> Int
fact xs = foldr (*) 1 xs

--Ejercicio 12:
and' :: [Bool] -> Bool
and' [] = False
and' xs = foldl (&&) True xs

--Ejercicio 13:
tam :: [a] -> Int
tam [] = 0
tam xs = foldr (+) 0 (listUno xs)

listUno :: [a] -> [Int]
listUno [] = []
listUno (x:xs) = 1:(listUno xs)

--Otra forma:
contar :: a -> Int -> Int --acc: acumulador
contar _ acc = acc + 1

tam' :: [a] -> Int
tam' xs = foldr contar 0 xs

--Ejercicio 14:
sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores xs = [succ x | x <- xs]

--Ejercicio 15:
cuadList' :: [Int] -> [Int]
cuadList' [] = []
cuadList' xs = [x^2 | x <- xs]

--Ejercicio 16:
listPares :: [Int] -> [Int]
listPares [] = []
listPares xs = [x | x <- xs, (mod x 2) == 0, x > 10]

--Ejercicio 17:
divs :: Int -> [Int]
divs n = [x | x <- divisores n]

divs' :: Int -> [Int]
divs' n = [x | x <- [1..n], (mod n x) == 0]

--Ejercicio 18:
enLista :: (Eq a) => a -> [a] -> Bool
enLista n [] = False
enLista n (x:xs) = if n == x then True else enLista n xs

todosOcurrenEn :: (Eq a) => [a] -> [a] -> Bool
todosOcurrenEn [] [] = True
todosOcurrenEn xs [] = False
todosOcurrenEn [] ys = True
todosOcurrenEn (x:xs) ys = if enLista x ys then True && (todosOcurrenEn xs ys) else False

todosOcurrenEn2 :: (Eq a) => [a] -> [a] -> Bool
todosOcurrenEn2 [] [] = True
todosOcurrenEn2 xs [] = False
todosOcurrenEn2  [] ys = False
todosOcurrenEn2 xs ys = and [x `elem` ys | x <- xs]

--Ejercicio 19:
primo' :: Int -> Bool
primo' 1 = False
primo' n = and [mod n i /= 0 | i <- [2..(n-1)]]

listaPrimos :: Int -> [Int]
listaPrimos 0 = []
listaPrimos x
         | primo x == True = x:(listaPrimos (x-1))
         | otherwise = listaPrimos (x-1) 

--Función principal:
primos :: Int -> [Int]
primos n = [x | x <- listaPrimos n]

--Otra Forma:
primos' :: Int -> [Int]
primos' n = [x | x <-[2..n], primo' x]

--Ejercicio 20:
pCartesiano :: [Int] -> [Int] -> [(Int, Int)]
pCartesiano [] [] = []
pCartesiano xs [] = []
pCartesiano [] ys = []
pCartesiano xs ys = [(x, y) | x <- xs, y <- ys]

--Ejercicio 21:
ocurrencias :: (Eq a) => a -> [a] -> Int
ocurrencias _ [] = 0
ocurrencias n xs = sum (listUno [x | x <- xs, x == n])

--Otra forma:
ocurrencias' :: (Eq a) => a -> [a] -> Int
ocurrencias' _ [] = 0
ocurrencias' n xs = length [x | x <- xs, x == n]

--Ejercicio 22:
split2 :: [a] -> [([a],[a])] 
split2 xs = [auxs x xs | x <- [0..length xs]]

auxs :: Int -> [a] -> ([a],[a])
auxs _ [] = ([], [])
auxs 0 xs = ([], xs)
auxs n xs = (take n xs, tirar xs n)

tirar :: [a] -> Int -> [a]
tirar [] n = []
tirar xs 0 = xs
tirar (x:xs) n = tirar xs (n-1)

--Ejercicio 23:
sumSegmentos :: [Int] -> Int
sumSegmentos [] = 0
sumSegmentos xs = sum [sum (take x xs) | x <- [1..length xs]]

--Ejercicio 24:
linfPares :: [Int]
linfPares = [x | x <- [1..], (mod x 2) == 0]

