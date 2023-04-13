--Ejercicio 1:
linf = 1:linf 

--Ejercicio 2:
listNat :: Int -> [Int]
listNat n = n:(listNat (n + 1))

--Ejercicio 3:
pListNat :: Int -> [Int]
pListNat 0 = []
pListNat n = pListNat(n-1) ++ [n]

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

--Ejercicio 8:
--Función auxiliar:
sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + (sumatoria xs)

sumCuad :: [Int] -> Int
sumCuad xs = sumatoria(cuadList xs)

--Ejercicio 9:
listSucc :: [Int] -> [Int]
listSucc [] = []
listSucc (x:xs) = (succ x):(listSucc xs)

--Ejercicio 10:
