--Ejercicio 22:
split2 :: [a] -> [([a],[a])] 
split2 xs = [auxs x xs | x <- [0..length xs]]

auxs :: Int -> [a] -> ([a],[a])
auxs _ [] = ([], [])
auxs 0 xs = ([], xs)
auxs n xs = (take n xs, drop n xs)

split3 :: [a] -> [([a], [a], [a])] --Devuelve todas las formas de partir a una lista en 3
split3 [] = error "Lísta vacía!"
split3 xs = [(as, bs, cs) | (as, ys) <- split2 xs, (bs, cs) <- split2 ys]

--Ejercicio 5):

--Segundo Inciso:
subSeg :: (Eq a) => [a] -> [a] -> Bool
subSeg _ [] = True
subSeg xs ys = or [bs == ys | (as, bs, cs) <- split3 xs]

--Tercer Inciso:
subSegFinal :: (Eq a) => [a] -> [a] -> Bool
subSegFinal _ [] = True
subSegFinal xs ys = or [bs == ys | (as, bs) <- split2 xs]

--Cuarto Inciso:
sumSegMax :: [Int] -> Int
sumSegMax xs = maximum [sum bs | (as, bs, cs) <- split3 xs]