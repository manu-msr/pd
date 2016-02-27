-- Alfaro Mendoza Yoshua Ian, Patlani Aguilar Luis Angel
-- -----------------------------------
-- Programación Declarativa, 2016-2 --
-- Práctica 2                       --
--------------------------------------
import Data.Char

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