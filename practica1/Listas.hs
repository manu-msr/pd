-- -----------------------------------
-- Programación Declarativa, 2016-2 --
-- Práctica 2                       --
-- Sección 3: Listas                --
--                                  --
-- Integrantes:                     --
-- 1. Alfaro Mendoza Yoshua Ian     --
-- 2. Patlani Aguilar Luis Ángel    --
-- 3. Soto Romero Manuel            --
--------------------------------------
module Listas where

------------------------------------------------------------------------------
-- 1. Define una función atN que toma una lista y un número n y devuelve el --
-- elemento en la n-ésima posición de la lista.                             --
------------------------------------------------------------------------------
atN :: [a] -> Int -> a
atN [] _ = error "Error"
atN (x:xs) 0 = x
atN (x:xs) n = atN xs (n-1)

-----------------------------------------------------------------------------------
-- 2. Da la definición de una función que selecciona los k primeros elementos de --
-- una lista, donde k es el elemento más pequeño de la lista.                    --
-----------------------------------------------------------------------------------
kprimeros :: [a] -> Int -> [a]
--kprimeros [] _ = error "Error"
kprimeros _ 0 = []
kprimeros (x:xs) n = [x] ++ (kprimeros xs (n-1))

--------------------------------------------------------------------------------
-- 3. Define una función que dada una lista [a0, a1, a2, ..., am, an, ao, ap] --
-- devuelve una lista de pares cuyos elementos son (a0, ap) (a1, ap) (a1, a0) --
-- (a2, an)... ¿Cómo tratar el caso en la la lista dada tenga una longitud    --
-- impar? Incluye esa observación en tu implementación.                       --
--------------------------------------------------------------------------------