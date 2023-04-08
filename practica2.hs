--Práctica 2

--Ejercicio 2:
hd :: [a] -> a
hd [] = error "lista vacía!"
hd (x:xs) = x

tl :: [a] -> [a]
tl [] = []
tl (x:xs) = xs

last' :: [a] -> a
last' [] = error "Lista vacía!"
last' [x] = x
last' (x:xs) = last xs

init' :: [a] -> [a]
init' [] = []
init' [x] = []
init' (x:xs) = x:(init' xs)

--Ejercicio 3:
maxTres :: Int -> Int -> Int -> Int
maxTres x y z = max x (max y z)

--Ejercicio 4:
concatenar :: [a] -> [a] -> [a]
concatenar [] [] = []
concatenar xs [] = xs
concatenar [] ys = ys
concatenar (x:xs) ys = x:(concatenar xs ys)

--Otra forma:
conc :: [a] -> [a] -> [a]
conc xs [] = xs
conc [] ys = ys
conc (x:xs) ys = x:(conc xs ys)

tomar :: [a] -> Int -> [a]
tomar [] n = []
tomar xs 0 = []
toamr (x:xs) n = x:(tomar xs (n-1))

tirar :: [a] -> Int -> [a]
tirar [] n = []
tirar xs 0 = []
tirar (x:xs) n = tirar xs (n-1)

--concFinal :: a -> [a] -> [a]
--concFinal n [] = [n]
--concFinal n (x:xs) = x:(concFinal xs)

--Ejercicio 5:
abs' :: Int -> Int
abs' x
  | x < 0 = (-x)
  |otherwise = x

--Ejercicio 6:
edad :: Int -> Int -> Int -> Int -> Int -> Int -> Int
edad d1 m1 a1 d2 m2 a2
    | m2 < m1 = a2 - a1 - 1
    | m1 == m2 && d2 < d1 = a2 - a1 - 1
    | otherwise = a2 - a1

--Ejercicio 7:
xor :: Bool -> Bool -> Bool
xor True True = False
xor True False = True
xor False True = True
xor False False = False

xor2 :: Bool -> Bool -> Bool 
xor2 a b 
  | a == b = False
  | otherwise = True

--Ejercicio 8:          --Esta más o menos hecha (no sé que tan correcta sea)
esPrimo :: Int -> Bool
esPrimo n
  | n == 4 = False
  | n < 5 = True
  | n > 5 = mod n 2 /= 0 && mod n 3 /= 0 && mod n 5 /= 0

primo :: Int -> Bool
primo 1 = False
primo n = and [mod n i /= 0 | i <- [2..(n-1)]]

--Ejercicio 9:
listaPrimos :: Int -> [Int]
listaPrimos 0 = []
listaPrimos x
         | primo x == True = x:(listaPrimos (x-1))
         | otherwise = listaPrimos (x-1) 

--Ejercicio 10:          
reversaLista :: [a] -> [a]
reversaLista [] = []
reversaLista (x:xs) = reversaLista xs ++ [x]

--Ejercicio 11:
iguales :: (Eq a) => [a] -> [a] -> Bool
iguales [] [] = True
iguales (x:xs) (y:ys) = if x == y then iguales xs ys else False

--Ejercicio 12:
palindromo :: (Eq a) => [a] -> Bool
palindromo [] = True
palindromo xs = iguales xs (reverse xs)

--Ejercicio 13:
cantRaices :: Int -> Int -> Int -> Int
cantRaices a b c
    | ((b^2) - 4 * a * c) > 0 = 2
    | ((b^2) - 4 * a * c) == 0 = 1
    |otherwise = 0
