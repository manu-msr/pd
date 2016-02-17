-- ------------------------------------
-- Programación Declarativa, 2016-2  --
-- Práctica 2                        --
-- Sección 4: Listas por comprensión --
--                                   --
-- Integrantes:                      --
-- 1. Alfaro Mendoza Yoshua Ian      --
-- 2. Patlani Aguilar Luis Ángel     --
-- 3. Soto Romero Manuel             --
---------------------------------------
module ListasComprension where

---------------------------------------------------------------------
-- 1. Reescribe las siguientes listas como listas por comprensión: --
---------------------------------------------------------------------

-- a) [0, 1, 3, 7, 15, 31, 63, ...]
lista1 = [(div (-2 + 2^n) 2) | n <- [1..]]

-- b) [(3,4), (7,8), (11, 12), (15,16), ...]
lista2 = [(4*x-1, 4*x) | x <- [1..]]

-- c) [51, 58, 65, 72, 79, 86, 93, 100]
lista3 = [7*x + 2 | x <- [7..14]]

-- d) ternas pitagóricas
lista4 = [(a, b, c) | a <- [1..100], b <- [1..100], c <- [1..100], a^2 + b^2 == c^2]