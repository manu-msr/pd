module Clases where

l1 = [[1], [2], [3], [4]]
l2 = [['a'], ['b'], ['c']]

f :: [[b]] -> [[Char]]
f xs = [xs | y <- xs, xs <- l2, x <- xs]

intervalo :: Integer -> Integer -> [Integer]
intervalo m t
	| t < m = []
	| otherwise = m:(intervalo (m+1) t)

----------------------

intercalar x [] = [[x]]
intercalar x (y:ys) = (x:y:ys):[(y:zs) | zs <- (intercalar x ys)]

permutaciones [] = [[]]
permutaciones (x:xs) = [ys | zs <- permutaciones xs, ys <- (intercalar x zs)]

inits :: [a] -> [[a]]
inits [] = []
inits (x:xs) = [x]:[x:ys | ys <- inits xs]

--zip' :: [a] -> [b] -> [(a,b)]
--zip' xs ys = [(a,b) | a <- xs, b <- ys

zip2 xs ys = [(a,b) | i <- [0..(min (length xs) (length ys)-1)], a <- [enposicion xs i], b <- [enposicion ys i]]

enposicion :: [a] -> Int -> a
enposicion [] n = error "no"
enposicion (x:xs) 0 = x
enposicion (x:xs) n = enposicion xs (n-1)

unzip2 :: [(a, b)] -> ([a], [b])
unzip2 xs = ([fst y | y <- xs],[snd y | y <- xs])

unzip'' [] = ([], [])
unzip'' ((a,b):xs) = let z = unzip'' xs in ((a:fst z), (b:snd z))