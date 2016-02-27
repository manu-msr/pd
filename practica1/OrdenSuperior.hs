-- -----------------------------------------
-- Programación Declarativa, 2016-2       --
-- Práctica 2                             --
-- Sección 5: Funciones de orden superior --
--                                        --
-- Integrantes:                           --
-- 1. Alfaro Mendoza Yoshua Ian           --
-- 2. Patlani Aguilar Luis Ángel          --
-- 3. Soto Romero Manuel                  --
--------------------------------------------
module OrdenSuperior where

---------------------------------------------------------------------------------
-- 1. Funcion que devuelve una lista con los primeros n números de tribonacci: --
---------------------------------------------------------------------------------

-- Tribonacci
tribList :: Int -> [Int]
tribList n = tribAux n (take (n + 1) [1,1,1])

-- Función auxiliar
tribAux :: Int -> [Int] -> [Int]
tribAux n (x:xs)
	| n > 2 = (x:(tribAux (n - 1) (xs ++ [foldr (+) 0 (x:xs)])))
	| otherwise = (x:xs)