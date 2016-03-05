-- Alfaro Mendoza Yoshua Ian
-- Patlani Aguilar Luis Ángel
-- Soto Romero Manuel
-- Programación Declarativa, 2016-2 - Práctica 2
module Practica2 where

import Data.Char

-- 1. Función que elimina los elemento duplicados adyacentes de una lista
-- dejando únicamente una aparición de cada elemento.
elimDup :: Eq a => [a] -> [a]
elimDup [] = []
elimDup l = foldr (\x (y:ys) -> if(x /= y) then (x:(y:ys)) else (y:ys)) [last l] l

-- 2. Función que devuelve la mayor de las sumas de las sublistas no vacías
-- de elementos consecutivos de una lista.
maxsumas :: [Int] -> Int
maxsumas [] = 0
maxsumas xs = maxLst (map sum (sublistas xs))

-- Funcion auxiliar que devuelve sublistas consecutivas de una lista.
sublistas :: [Int] -> [[Int]]
sublistas [] = []
sublistas (x:xs) = (xsublists [x] xs)++(sublistas xs)

-- Función auxiliar que genera las sublistas que comienzan con x.
xsublists :: [Int] -> [Int] -> [[Int]]
xsublists xs [] = [xs]
xsublists xs (y:ys) = xs:(xsublists (xs++[y]) ys)

-- Función auxiliar que encuentra el valor máximo en una lista.
maxLst :: [Int] -> Int
maxLst [] = error "Lista vacía"
maxLst [x] = x
maxLst (x:xs) = max x (maxLst xs)

-- 3. Función que toma una lista y regresa otra con los elementos que aparecen
-- una única vez en la original.
unicosEnLista :: Eq a => [a] -> [a]
unicosEnLista [] = []
unicosEnLista (x:xs)
   | pertenece x xs = unicosEnLista (quita x xs)
   | otherwise = x:(unicosEnLista xs)

-- Función auxiliar que determina si un elemento está contenido en una lista.
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece e (x:xs)
   | e == x = True
   | otherwise = pertenece e xs

-- Función auxiliar que quita todas las apariciones de un elemento en una lista
-- dada.
quita :: Eq a => a -> [a] -> [a]
quita _ [] = []
quita e (x:xs) 
   | e == x = quita e xs 
   | otherwise = x:(quita e xs)

-- 4. Función que devuelve el s-dígito de un número concatenado k veces consigo
-- mismo.
f :: Int -> Int -> Int
f n k = (sdigito n)*k

-- Función auxiliar que representa un número en forma de lista para manipular
-- sus dígitos individualmente.
digitos :: Int -> [Int]
digitos 0 = []
digitos x = digitos (x `div` 10)++[x `mod` 10]

-- Función auxiliar que calcula el s-digito de un número.
sdigito :: Int -> Int
sdigito n = sum (digitos n)

-- 5. Función que encuentra un número p talta que n = p + p', donde p' = iflip p
iflip :: Int -> Int
iflip n = (foldl (\ x y -> x * 10 + y) 0 (digits n 10))

-- Funcion auxiliar que devuelve la lista de los digitos de un entero, dada una 
-- base.
digits :: Int -> Int -> [Int]
digits 0 b = []
digits x b
   | x < b = [x]
   | otherwise = m:(digits (div (x - m) b) b)
   where m = (mod x b)

-- Funcion que indica si existe para un natural, un entero que sumado con iflip
-- del entero de como resultado el natural.
existeIF :: Int -> Int
existeIF n
   | (length p) > 0 = (head p)
   | otherwise = 0
   where p = filter (\ x -> (x + (iflip x)) == n) [0 .. n]

-- 6. Función que recibe una lista y regresa una lista de pares (k,x) donde k es
-- el número de apariciones consecutivas de x en la lista.
apars :: Eq a => [a] -> [(Int, a)]
apars [] = []
apars (x:xs) = (na + 1, x):(apars (drop na xs))
   where na = (aparsN x xs)

-- Función auxiliar que devuelve el número de apariciones de un elemento en una lista.
aparsN :: Eq a => a -> [a] -> Int
aparsN e [] = 0
aparsN e (x:xs)
   | x == e = 1 + (aparsN e xs)
   | otherwise = 0

-- 7. Función que recibe una oración computesta por una o más palabras separadas
-- por un espacio y devuelve la cadena escondida en el mensaje.
hiddenW :: [Char] -> [Char]
hiddenW [] = []
hiddenW l = (splitGet 0 word):(hiddenW tail)
   where (word, tail) = (splitW [] l)

-- Funcion auxiliar que separa la primer palabra de una cadena.
splitW :: [Char] -> [Char] -> ([Char],[Char])
splitW w [] = (w,[])
splitW w (' ':xs) = (w, xs)
splitW w (x:xs) = splitW (w++[x]) xs

-- Funcion auxiliar que separa el primer número de una palabra
-- y devuelve el caracter de la palabra con la posicion del numero.
splitGet :: Int -> [Char] -> Char
splitGet n (x:xs)
   | isDigit x = splitGet (n * 10 + (digitToInt x)) xs
   | otherwise = takePos n (x:xs)

-- Funcion auxiliar que toma el caracter de una cadena con una posicion dada.
takePos :: Int -> [Char] -> Char
takePos n [] = ' '
takePos n (x:xs)
   | n > 0 = takePos (n - 1) xs
   | otherwise = x