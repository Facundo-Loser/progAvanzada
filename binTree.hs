data BinTree a = Nil | Node (BinTree a) a (BinTree a) deriving (Show, Eq)

--Función que suma todos los elementos de un arbol binario de enteros 
sumTree :: BinTree Int -> Int
sumTree Nil = 0
sumTree (Node hi r hd) = r + (sumTree hi)+(sumTree hd)

--Función que dado un arbol binario, devulve su altura:
alturaArbol :: BinTree a -> Int
alturaArbol Nil = 0
alturaArbol (Node hi r hd) = 1 + (max (alturaArbol hi) (alturaArbol hd))

--Ejemplo para probar la función 
--(Node (Node Nil 4 Nil) 2 (Node (Node Nil 8 Nil) 6 (Node Nil 7 Nil)))                               (Debe devolver 3)

--Función que devulve True si un arbol es balanceado o False si no lo es.
arbolBalanceado :: BinTree a -> Bool
arbolBalanceado Nil = True
arbolBalanceado (Node hi r hd) = (alturaArbol hi) == (alturaArbol hd)

--Ejemplos para probar:
--(Node (Node Nil 4 Nil) 2 (Node Nil 6 Nil))                                                         (Debe devolver True)
--(Node (Node (Node Nil 7 Nil) 4 (Node Nil 8 Nil)) 2 (Node (Node Nil 9 Nil) 6 (Node Nil 10 Nil)))    (Debe devolver True)
--(Node (Node Nil 4 Nil) 2 (Node (Node Nil 9 Nil) 6 (Node Nil 10 Nil)))                              (Debe devolver Falso)

--Función que compara si dos arboles son iguales:
iguales :: (Eq a) => BinTree a -> BinTree a -> Bool
iguales Nil Nil = True
iguales (Node hi1 r1 hd1) Nil = False
iguales Nil (Node hi2 r2 hd2) = False
iguales (Node hi1 r1 hd1) (Node hi2 r2 hd2)
        | r1 == r2 = (iguales hi1 hi2) && (iguales hd1 hd2)
        | otherwise = False


--Implementación sin terminar de un mejor Show para BinTree.

--instance (Show a) => Show (BinTree a) where
--show Nil = "<>"
--show (Node hi r hd) = "<" ++ show hi ++ ", <" ++ show r ++ ">," ++ show hd ++ ">"