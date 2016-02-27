-- Alfaro Mendoza Yoshua Ian, Patlani Aguilar Luis Angel
-- -----------------------------------
-- Programación Declarativa, 2016-2 --
-- Práctica 2                       --
--------------------------------------
import Data.Char

-- 1. elimDup
elimDup :: Eq a => [a] -> [a]
elimDup [] = []
elimDup (x:xs) = foldl f [x] xs
	where f x (y:ys) 

-- 2. maxsumas
maxsumas :: [Int] -> Int
maxsumas [] = 0;
maxsumas (x:xs) = 0

-- Funcion auxiliar que devuelve sublistas consecutivas de una lista.
sublistas :: [Int] -> [[Int]]
sublistas [] = []
sublistas (x:xs) = []

-- 3. unicos
unicos :: [a] -> [a]
unicos [] = []
unicos (x:xs) = []

-- 4. sdigit
sdigit :: Int -> Int
sdigit n
	| n < 10 = n
	| otherwise = sdigit n

-- 5. iflip
iflip :: Int -> Int
iflip n = 0

-- Funcion que indica si existe para un natural, un entero que sumado con iflip
-- del entero de como resultado el natural.
existIF :: Int -> Bool
existIF 0 = True

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

-- 7. hiddenW
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