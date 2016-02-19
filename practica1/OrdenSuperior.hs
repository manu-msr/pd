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

--------------------------------------------------------------------------------
-- 1. Funcion que devuelve una lista con los primeros n números de tribonacci: --
--------------------------------------------------------------------------------

-- tribonacci
tribList :: Int -> [Int]
tribList n = (tribRec n 1 1 1 [1])

-- Funcion auxiliar
tribRec :: Int -> Int -> Int -> Int -> [Int] -> [Int]
tribRec n p s t l
	| n < 3 = [1 | m <- [1..n]] ++ (reverse l)
	| otherwise = (tribRec (n - 1) pst p s (pst:l))
	where pst = (p + s + t)