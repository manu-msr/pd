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