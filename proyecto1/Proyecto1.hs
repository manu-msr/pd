--------------------------------------------------------------------------------
-- Universidad Nacional Autónoma de México                                    --
-- Facultad de Ciencias                                                       --
-- Programación Declarativa, 2016-2                                           --
-- Proyecto 1: Criptoaritmos                                                  --
-- Manuel Soto Romero                                                         --
--------------------------------------------------------------------------------
module Proyecto1 where

import Data.Char

--------------------------------------------------------------------------------
-- Esta función pide una ecuación al usuario. Las ecuaciones son binarias, es --
-- decir, son de la forma A+B=C. La única operación que soportan es la suma.  --
-- En esta función se pide una ecuación y después se procesa mediante las     --
-- funciones correspondientes.                                                --
--------------------------------------------------------------------------------
criptoaritmos =
   do
      putStr "Introduce una ecuacion: "
      ecuacion <- getLine
      putStrLn ("La ecuación introducida es: " ++ ecuacion)
      putStrLn ("Procesando...\n")
      let x = (identificaLetras ecuacion)
      if (length x) > 10 
         then -- No hay suficientes dígitos
            putStrLn ("No hay solución.")
         else
            if (length x) == 10
               then -- No es necesario encontrar las sublistas
                  putStrLn (muestraLista 1 (verifica (permutaciones [0..9]) x (encuentraOp1 ecuacion) (encuentraOp2 ecuacion) (encuentraRes ecuacion)))
               else
                  putStrLn (muestraLista 1 (verifica (permutaSubListas (sublistasN (length x) [0..9])) x (encuentraOp1 ecuacion) (encuentraOp2 ecuacion) (encuentraRes ecuacion)))

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
-- Función que coordina el cálculo para el resultado. Para ello recibe una    --
-- lista de enteros. La función que espera recibir está formada por las permu_--
-- taciones de las sublistas de tamaño n de la lista [0..9], donde n es el nú_--
-- mero de letras de la ecuación. Además, recibe, la lista de lestras, el ope_--
-- rando izquierdo, el operando derecho y el resultado. La función verifica   --
-- cada posible sublista, si esta es solución se agrega, de lo contrario se   --
-- sigue con el resto de las sublistas.                                       --
--------------------------------------------------------------------------------
verifica :: [[Int]] -> String -> String -> String -> String -> [[(Int, Char)]]
verifica [] _ _ _ _ = []
verifica (x:xs) letr izq der res
   | (verifica' aux izq der res) = aux:(verifica xs letr izq der res)
   | otherwise = verifica xs letr izq der res
      where aux = (zip x letr)

--------------------------------------------------------------------------------
-- Función que se continúa con el cálculo del resultado. Esta lista recibe pa_--
-- res de la forma (Int, Char), los cuales son propuestas para el resultado,  --
-- la función se encarga de llamar a otras funciones que confirmarán si son   --
-- resultado para la ecuación.                                                --
--------------------------------------------------------------------------------
verifica' :: [(Int, Char)] -> String -> String -> String -> Bool
verifica' a izq der res = revisa (aInt (construye a izq)) (aInt (construye a der)) (aInt (construye a res))

muestraLista :: Show a => Int -> [a] -> String
muestraLista _ [] = " "
muestraLista n (x:xs) = show n ++ " " ++ show x ++ "\n" ++ (muestraLista (n+1) xs)

--------------------------------------------------------------------------------
-- Función que se encarga de construir una lista de enteros, de acuerdo a los --
-- valores de la cadena original. Esto con el fin de verificar si los operan_ --
-- dos y el resultado son solución.                                           --
--------------------------------------------------------------------------------
construye :: [(Int, Char)] -> String -> [Int]
construye [] _ = []
construye _ [] = []
construye xs (y:ys) = (busca y xs):(construye xs ys)

--------------------------------------------------------------------------------
-- Función que busca el valor de cada letra de la cadena. Si no existe valor  --
-- para dicho símbolo, se devuelve -1.                                        --
--------------------------------------------------------------------------------
busca :: Char -> [(Int, Char)] -> Int
busca c ((a,b):xs)
   | c == b = a
   | otherwise = busca c xs 

--------------------------------------------------------------------------------
-- Una vez que se obtuvo la lista de números, debemos de operar con ella, para--
-- esto se convierte a entero.                                                --
--------------------------------------------------------------------------------
aInt :: [Int] -> Int
aInt xs = aIntAux xs ((length xs)-1)

--------------------------------------------------------------------------------
-- Para convertir la lista a enteros, utilizamos notación exponencial.        --
--------------------------------------------------------------------------------
aIntAux :: [Int] -> Int -> Int
aIntAux [] _ = 0
aIntAux (x:xs) p = (x * (potenciaInt 10 p)) + (aIntAux xs (p-1))

--------------------------------------------------------------------------------
-- Función para realizar la potencia. Se usa esta función pues el operador ** --
-- devuelve datos de tipo flotante, con lo cual el operador * indica que no es-- 
--  una operación segura.                                                     --
--------------------------------------------------------------------------------
potenciaInt :: Int -> Int -> Int
potenciaInt _ 0 = 1
potenciaInt a b = a * (potenciaInt a (b-1))

--------------------------------------------------------------------------------
-- Función que revisa si los valores propuestos satisfacen la ecuación.       --
--------------------------------------------------------------------------------
revisa :: Int -> Int -> Int -> Bool
revisa a b c
   | a + b == c = True
   | otherwise = False

--------------------------------------------------------------------------------
-- Función que realiza las permutaciones de una lista. Para ello se axulia de --
-- la función intercalar.                                                     --
--------------------------------------------------------------------------------
permutaciones :: [a] -> [[a]]
permutaciones [] = [[]]
permutaciones (x:xs) = [ys | zs <- permutaciones xs, ys <- (intercalar x zs)]

--------------------------------------------------------------------------------
-- Función que va moviendo un elemento por toda la lista. Por ejemplo, para la--
-- lista [1,2,3] con 1 devuelve [1.2,3]. [2,1,3] y [2,3,1].                   --
--------------------------------------------------------------------------------
intercalar :: a -> [a] -> [[a]]
intercalar x [] = [[x]]
intercalar x (y:ys) = (x:y:ys):[(y:zs) | zs <- (intercalar x ys)]

--------------------------------------------------------------------------------
-- Función que calcula el conjunto potencia de una lista.                     --
-- Esta función nos permitirá elegir los subconjuntos de tamaño n.            --
--------------------------------------------------------------------------------
conjPot :: [a] -> [[a]]
conjPot [] = [[]]
conjPot (x:xs) = acc ++ map (x:) acc 
   where acc = conjPot xs

--------------------------------------------------------------------------------
-- Función que encuentra sublistas de tamaño n. Para ello recorre el conjunto --
-- potencia y se queda con los que sean del tamaño.                           --
--------------------------------------------------------------------------------
sublistasN :: Int -> [a] -> [[a]]
sublistasN _ [] = [[]]
sublistasN n xs = [ys | ys <-conjPot xs, (length ys) == n]

--------------------------------------------------------------------------------
-- Función que simplemente permuta cada una de las sublistas. Se utiliza para --
-- permutar las sublistas.                                                    --
--------------------------------------------------------------------------------
permutaSubListas :: [[a]] -> [[a]]
permutaSubListas [] = []
permutaSubListas (x:xs) = (permutaciones x)++(permutaSubListas xs)
