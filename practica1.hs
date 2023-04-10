--Práctica 1:

--Ejercicio 5:
esPar :: Int -> Bool
esPar n = (mod n 2) == 0

--Ejercicio 6:
multiploTres :: [Int] -> Bool
multiploTres n = (mod (sum n) 3) == 0

--Ejercicio 8:              (Hay que corregirla)
numList :: Int -> [Int]
numList n
      | n == 0 = [0]
      | n > 0 = (div n 10):numList (mod n 10)

--Ejercicio 9:
palindromo :: String -> Bool
palindromo [] = True
palindromo xs = xs == reverse xs