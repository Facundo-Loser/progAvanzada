--Ejercicio 1:
data Nat = Zero | Succ Nat deriving Show

--Ejercicio 2:
natToInt :: Nat -> Int 
natToInt Zero = 0
natToInt (Succ n) = 1 + (natToInt n)

--Ejercicio 3:
intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat n = (Succ(intToNat (n-1)))

--Ejercicio 4:
sumaNat :: Nat -> Nat -> Nat
sumaNat n m = intToNat ((natToInt n) + (natToInt m))

--Ejercicio 5: (hecho en el programa binTree.hs)