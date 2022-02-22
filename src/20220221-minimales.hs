{-
Definir la función

   minimales :: Ord a => [[a]] -> [[a]]

tal que (minimales xss) es la lista de los elementos de xss que no están contenidos en otros elementos de xss. Por ejemplo,

   minimales [[1,3],[2,3,1],[3,2,5]]        ==  [[2,3,1],[3,2,5]]
   minimales [[1,3],[2,3,1],[3,2,5],[3,1]]  ==  [[2,3,1],[3,2,5]]
   map sum (minimales [[1..n] | n <- [1..300]])  ==  [45150]

https://www.glc.us.es/~jalonso/exercitium/determinacion-de-los-elementos-minimales/

-}

module Minimales where

import Data.List ((\\))

minimales :: Ord a => [[a]] -> [[a]]
minimales [] = []
minimales (xs : xss)
  | any (xs ⊆) xss = minimales xss
  | otherwise = xs : minimales (filter (⊈ xs) xss)
  where
    xs ⊆ ys = null $ xs \\ ys
    xs ⊈ ys = not $ xs ⊆ ys
