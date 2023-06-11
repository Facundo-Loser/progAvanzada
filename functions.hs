--Funciones sin completar de tp de avanzada:

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

mejorJug :: Estado -> Int
mejorJug (j, k) | k == 1 = 1
                | k == 2 = 1
				| k == 3 = 3
				| k == 4 = 4
				| k > 4 = if j == C then aux (C, k) else aux (H, k)


--Se supone que k > 4 para llamar a esta función 
--Pd: ya se que lo que hice con los if es un crimen de guerra pero no se me ocurria una forma mas elegante de hacerlo.
aux :: Estado -> Int 
aux (j, k) | j == C = if evalEstado (H, (k-1)) == CGano then 1 else if evalEstado (H, (k-3)) == CGano then 3 else if evalEstado (H, (k-4)) == CGano then 4 else 1 
	       | j == H = if evalEstado (C, (k-1)) == CPerdio then 1 else if evalEstado (C, (k-3)) == CPerdio then 3 else if evalEstado (H, (k-4)) == CPerdio then 4 else 1


-- juegosGanadores k, calcula todos los comienzos ganadores para la computadora hasta con k piedras
-- por ejemplo, juegosGanadores 10 = [2,7,9]
juegosGanadores :: Int -> [Int]
juegosGanadores i = [x | x <- [0..i], evalEstado(C, x) == CGano] 