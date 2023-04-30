data Nat = Zero | Succ Nat deriving Eq

natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ n) = 1 + (natToInt n)

intToNat :: Int -> Nat
intToNat 0 = Zero
intToNat n = Succ (intToNat (n-1))

sumNat :: Nat -> Nat -> Nat
sumNat n m = intToNat ((natToInt n) + (natToInt m))

--Función que dice si un Nat es primo:
natPrimo :: Nat -> Bool
natPrimo n = primo (natToInt n)

primo :: Int -> Bool
primo 1 = False
primo n = and [mod n i /= 0 | i <- [2..(n-1)]]

--Función que dado un Nat genera una lista desde Zero hasta el Nat dado:
listaNat :: Nat -> [Nat]
listaNat Zero = [Zero]
listaNat (Succ n) = map (intToNat) [x | x <- [0..(natToInt n)]]

--Función que divide dos Nat:
divNat :: Nat -> Nat -> Nat
divNat _ Zero = error "división por cero"
divNat Zero _ = Zero
divNat (Succ n) (Succ m) = intToNat (div (natToInt (Succ n)) (natToInt (Succ m)))

--Función que dice si un Nat esta en la lista:
buscarNat :: Nat -> [Nat] -> Bool
buscarNat _ [] = False
buscarNat (Succ n) (x:xs) = if (Succ n) == x then True else buscarNat (Succ n) xs

--Ejemplo para probar: buscarNat (Succ Zero) (map (intToNat) [0..20])

--Función que multiplica dos Nat:
multNat :: Nat -> Nat -> Nat
multNat Zero Zero = Zero
multNat Zero _ = Zero
multNat _ Zero = Zero
multNat (Succ n) (Succ m) = intToNat ((natToInt (Succ n)) * (natToInt (Succ m)))

--Ejemplo para probar: multNat (intToNat 2) (intToNat 4)


--Instacia para mostrar los Nat como números:
instance (Show Nat) where
    show Zero = "0"
    show (Succ n) = show (natToInt (Succ n))