-- -----------------------------------
-- Programación Declarativa, 2016-2 --
-- Práctica 2                       --
-- Sección 2: Números               --
--                                  --
-- Integrantes:                     --
-- 1. Alfaro Mendoza Yoshua Ian     --
-- 2. Patlani Aguilar Luis Ángel    --
-- 3. Soto Romero Manuel            --
--------------------------------------
module Numeros where

------------------------------------------------------------------------------
-- 1. Define la funcion max que devuelve el número mayor dados dos números. --
------------------------------------------------------------------------------
max' :: Int -> Int -> Int
max' a b = if a > b then a else b

------------------------------------------------------------------------------------
-- 2. Define la función maxthree que devuelve el número mayor dados tres números. --
------------------------------------------------------------------------------------
maxthree :: Int -> Int -> Int -> Int
maxthree a b c
	| a > b && a > c = a
	| b > a && b > c = c
	| otherwise = c

--------------------------------------------------------------------
-- 3. Define una función que calcule el promedio de tres números. --
--------------------------------------------------------------------
promedio :: Int -> Int -> Int -> Int
promedio a b c = (div (a + b + c) 3)

-------------------------------------------------------------------------------
-- 4. Define la función gtaveragethree que recibe tres enteros y devuelve el --
-- número que es mayor al promedio de los tres.                              --
-------------------------------------------------------------------------------
gtaveragethree :: Int -> Int -> Int -> Int
gtaveragethree a b c
	| a >= prom = a
	| b >= prom = b
	| otherwise = c
	where prom = promedio a b c