import Prelude hiding (show)
import qualified Prelude

data BinTree a = Nil | Node (BinTree a) a (BinTree a) deriving Eq

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

--Función que genera un arbol de enteros de forma decreciente a partir de un número:
generarArbol :: Int -> BinTree Int
generarArbol 0 = (Node Nil 0 Nil)
generarArbol 1 = (Node Nil 1 Nil)
generarArbol n = (Node (generarArbol (n-1)) n (generarArbol (n-2)))

--Función que dice si un elem está en un árbol binario:
buscarElem :: (Eq a) => BinTree a -> a -> Bool
buscarElem Nil _ = False
buscarElem (Node Nil r Nil) n = r == n
buscarElem (Node hi r hd) n = if n == r then True else buscarElem hi n || buscarElem hd n

--Función que duvuelve la cantidad de nodos de un árbol binario:
cantNodos :: BinTree a -> Int
cantNodos Nil = 0
cantNodos (Node Nil r Nil) = 1
cantNodos (Node hi r hd) = 1 + (cantNodos hi) + (cantNodos hd)

--Función que invierte un árbol binario:
invertir :: BinTree a -> BinTree a
invertir Nil = Nil
invertir (Node Nil r Nil) = (Node Nil r Nil)
invertir (Node hi r hd) = (Node (invertir hd) r (invertir hi))


--Implementación sin terminar de un mejor Show para BinTree.

--instance (Show a) => Show (BinTree a) where
--show Nil = "<>"
--show (Node Nil r Nil) = "<" ++ show Nil ++ "<" ++ show r ++ ">" ++ show Nil ">"
--show (Node hi r hd) = "<" ++ show hi ++ ", <" ++ show r ++ ">," ++ show hd ++ ">"