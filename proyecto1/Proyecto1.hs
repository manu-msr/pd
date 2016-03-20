--------------------------------------------------------------------------------
-- Universidad Nacional Autónoma de México                                    --
-- Facultad de Ciencias                                                       --
-- Programación Declarativa, 2016-2                                           --
-- Proyecto 1: Criptoaritmos                                                  --
-- Manuel Soto Romero                                                         --
--------------------------------------------------------------------------------
module Proyecto1 where

import Data.Char

-- Un tipo incógnita de la forma id = valor, ejemplo a = 2
type Incognita = (Char, Int)

--------------------------------------------------------------------------------
-- Interacción con el usuario. En esta sección se agregan las funciones para  --
-- pedir datos al usuario y arrojar resultados.                               --
--------------------------------------------------------------------------------
criptoaritmos =
   do
      putStr "Introduce una ecuacion: "
      ecuacion <- getLine
      putStrLn ("La ecuación introducida es: " ++ ecuacion)

--------------------------------------------------------------------------------
-- Función que encuentra el primer operador de una ecuación                   --
--------------------------------------------------------------------------------
encuentraOp1 :: String -> String
encuentraOp1 [] = []
encuentraOp1 (x:xs)
   | x == '+' = []
   | otherwise = x:(encuentraOp1 xs)

--------------------------------------------------------------------------------
-- Función que encuentra el segundo operador de una ecuación                  --
--------------------------------------------------------------------------------
encuentraOp2 :: String -> String
encuentraOp2 ec = op2aux ec False

--------------------------------------------------------------------------------
-- Función auxiliar para encontrar el segundo operador de la ecuación.        --
--------------------------------------------------------------------------------
op2aux :: String -> Bool -> String
op2aux [] _ = []
op2aux (x:xs) b
   | x /= '+' && b == False = (op2aux xs False)
   | x == '+' = (op2aux xs True)
   | x == '=' = []
   | otherwise = x:(op2aux xs True)

--------------------------------------------------------------------------------
-- Función que encuentra el resultado de la ecuación.                         --
--------------------------------------------------------------------------------
encuentraRes :: String -> String
encuentraRes ec = resaux ec False

--------------------------------------------------------------------------------
-- Función auxiliar para encontrar el resultado de la ecuación.
--------------------------------------------------------------------------------
resaux :: String -> Bool -> String
resaux [] _ = []
resaux (x:xs) b
   | x /= '=' && b == False = (resaux xs False)
   | x == '=' = (resaux xs True)
   | otherwise = x:(resaux xs True)

--------------------------------------------------------------------------------
-- Función que identifica las letras involucradas en la ecuación (sin         --
-- repeticiones)                                                              --
--------------------------------------------------------------------------------
identificaLetras :: String -> String
identificaLetras [] = []
identificaLetras (x:xs)
   | elem (toLower x) xs = identificaLetras xs
   | x == '+' || x == '=' = identificaLetras xs
   | otherwise = (toLower x):(identificaLetras xs)

--------------------------------------------------------------------------------
-- Función que encuentra las permutaciones posibles de una lista de la forma  --
-- [1..n].                                                                    --
--------------------------------------------------------------------------------
permutacionesN :: Int -> [[Int]]
permutacionesN n = permutaciones [1..n]

permutaciones :: [a] -> [[a]]
permutaciones [] = [[]]
permutaciones (x:xs) = [ys | zs <- permutaciones xs, ys <- (intercalar x zs)]

intercalar :: a -> [a] -> [[a]]
intercalar x [] = [[x]]
intercalar x (y:ys) = (x:y:ys):[(y:zs) | zs <- (intercalar x ys)]