--Funciones sin completar de tp de avanzada:

-- | La funcion otro Jugador, dado un jugador, devuelve el otro jugador, por ejemplo: otroJugador C = H
otroJugador :: Jugador -> Jugador
otroJugador n | n == C = H
              | n == H = C

-- | Dada una jugada (cantidad de piedras que se retiran) y un estado retorna el estado resultante, se deben controlar los casos de jugadas no posibles
hacerJugada :: Int -> Estado -> Estado
hacerJugada n (j, k) | (elem n jugadas) = (j, k - n)
                     | otherwise = error "jugada no válida!"

