--FUNCIONES DEL TP DE AVANZADA:

-- | La funcion otro Jugador, dado un jugador, devuelve el otro jugador, por ejemplo: otroJugador C = H
otroJugador :: Jugador -> Jugador
otroJugador n | n == C = H
              | n == H = C

-- | Dada una jugada (cantidad de piedras que se retiran) y un estado retorna el estado resultante, se deben controlar los casos de jugadas no posibles
hacerJugada :: Int -> Estado -> Estado
hacerJugada n (j, k) | (elem n jugadas) && (n <= k) = (j, k - n)
                     | otherwise = error "jugada no válida!"


-- | Calcula la mejor jugada para un estado dado, para el jugador dado.
-- Por ejemplo, mejorJug (H,3) debería devolver 3, ya que la mejor jugada para H cuando hay 3 piedras es retirar 3.
-- Ayuda: Tener en cuenta que el tipo Resultado implementa la clase Ord, es decir, tenemos
-- CPerdio < CGano. Es decir:
--
-- 	Para el caso mejorJug(C, k) tenemos que devolver la jugada que nos da el resultado maximo con respecto a C (es 
-- 	decir, la mejor jugada para la computadora).
-- 	
-- 	En el caso mejorJug (H, k) tenemos que devolver la jugada que nos da el valor minimo (es decir, consideramos 
-- 	la mejor jugada para H, que seria la peor para C).

--VERSIÓN 1: (no tan óptima)
mejorJug :: Estado -> Int
mejorJug (j, k) | k == 1 = 1
                | k == 2 = 1
                | k == 3 = 3
                | k == 4 = 4
                | k > 4 = if j == C then aux (C, k) else aux (H, k)

--VERSIÓN 2: (Un poco mejor)
mejorJug' :: Estado -> Int
mejorJug' (j, k) | k == 1 = 1
                 | k == 2 = 1
                 | k == 3 = 3
                 | k == 4 = 4
                 | k > 4 && j == C =  aux (C, k)
                 | k > 4 && j == H =  if elem k [i | i <- [1..k], not(elem i (juegosGanadores k))] then 4 else aux (H, k)      

--Estas dos versiones funcionan correctamente pero ninguna es lo suficientemente óptima como para hacer que la computadora gane siempre


--VERSION 3: (mejor que las dos anteriores creo)
mejorJug3 :: Estado -> Int
mejorJug3 (j, k) | k == 1 = 1
                 | k == 2 = 1
                 | k == 3 = 3
                 | k == 4 = 4
                 | k > 4 = aux2 (j, k)

aux2 :: Estado -> Int
aux2 (j, k) | j == C = maximum [x | x <- jugadas, evalEstado (H, (k-x)) == CGano]
            | j == H = minimum [x | x <- jugadas, evalEstado (C, (k-x)) == CPerdio]





--Se supone que k > 4 para llamar a esta función 
--Dado un estado se consideran todas las posibles jugadas y se llama a evalEstado para ver si en cada caso el humano o la computadora es ganador.
--Por ejemplo: dado (C, 10) las posibles jugadas son que C saque 1, 3 o 4 piedras: (H, 9), (H, 7), (H, 6), y luego de aca se llama a evalEstado con cada una.
--Pd: ya se que lo que hice con los if es un crimen de guerra pero no se me ocurria una forma mas elegante de hacerlo.
aux :: Estado -> Int 
aux (j, k) | j == C = if evalEstado (H, (k-1)) == CGano then 1 else if evalEstado (H, (k-3)) == CGano then 3 else if evalEstado (H, (k-4)) == CGano then 4 else 4 
           | j == H = if evalEstado (C, (k-1)) == CPerdio then 1 else if evalEstado (C, (k-3)) == CPerdio then 3 else if evalEstado (H, (k-4)) == CPerdio then 4 else 4


-- juegosGanadores k, calcula todos los comienzos ganadores para la computadora hasta con k piedras
-- por ejemplo, juegosGanadores 10 = [2,7,9]
juegosGanadores :: Int -> [Int]
juegosGanadores i = [x | x <- [1..i], evalEstado(H, x) == CGano] 


--FUNCIONES AUXILIARES QUE PODRÍAN LLEGAR A SER ÚTILES:

--Devuelve todas las jugadas posibles dado un estado:
jugs :: Estado -> [Estado]
jugs (j, k) = [(otroJugador j, k - i) | i<- jugadas, i<=k]  