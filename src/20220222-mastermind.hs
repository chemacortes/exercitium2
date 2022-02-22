{-
El Mastermind es un juego que consiste en deducir un código numérico formado por
una lista de números. Cada vez que se empieza una partida, el programa debe
elegir un código, que será lo que el jugador debe adivinar en la menor cantidad
de intentos posibles. Cada intento consiste en una propuesta de un código
posible que propone el jugador, y una respuesta del programa. Las respuestas le
darán pistas al jugador para que pueda deducir el código.

Estas pistas indican lo cerca que estuvo el número propuesto de solución a
través de dos valores: la cantidad de aciertos es la cantidad de dígitos que
propuso el jugador que también están en el código en la misma posición. La
cantidad de coincidencias es la cantidad de dígitos que propuso el jugador que
también están en el código pero en una posición distinta.

Por ejemplo, si el código que eligió el programa es el [2,6,0,7] el jugador
propone el [1,4,0,6], el programa le debe responder acierto (el 0, que está en
el código original en el mismo lugar, tercero), y una coincidencia (el 6, que
también está en el original, pero en la segunda posición, no en el cuarto como
fue propuesto). Si el jugador hubiera propuesto el [3,5,9,1], habría obtenido
como respuesta ningún acierto y ninguna coincidencia, ya que no hay números en
común con el código original. Si se obtienen cuatro aciertos es porque el
jugador adivinó el código y ganó el juego.

Definir la función

   mastermind :: [Int] -> [Int] -> (Int,Int)

tal que (mastermind xs ys) es el par formado por los números de aciertos y de
coincidencias entre xs e ys. Por ejemplo,

   mastermind [3,3] [3,2]          ==  (1,0)
   mastermind [3,5,3] [3,2,5]      ==  (1,1)
   mastermind [3,5,3,2] [3,2,5,3]  ==  (1,3)
   mastermind [3,5,3,3] [3,2,5,3]  ==  (2,1)
   mastermind [1..10^6] [1..10^6]  ==  (1000000,0)

https://www.glc.us.es/~jalonso/exercitium/mastermind/
-}

module Mastermind where

mastermind :: [Int] -> [Int] -> (Int, Int)
mastermind xs ys = foldl f (0, 0) zs
  where
    zs = zip xs ys
    f (aciertos, colocados) (x, y)
      | x == y = (aciertos + 1, colocados)
      | y `elem` xs = (aciertos, colocados + 1)
      | otherwise = (aciertos, colocados)
