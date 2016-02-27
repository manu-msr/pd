-- Alfaro Mendoza Yoshua Ian, Patlani Aguilar Luis Angel
-- -----------------------------------
-- Programación Declarativa, 2016-2 --
-- Práctica 2                       --
--------------------------------------
import Data.Char

-- 6. apars
apars :: Eq a => [a] -> [(a, Int)]
apars [] = []
apars (x:xs) = (x, (aparsN x xs)):(apars [e | e <- xs, e /= x])

-- Función auxiliar que devuelve el número de apariciones de un elemento en una lista.
aparsN :: Eq a => a -> [a] -> Int
aparsN e [] = 1
aparsN e (x:xs)
	| x == e = 1 + (aparsN e xs)
	| otherwise = (aparsN e xs)