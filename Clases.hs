module Clases where

l1 = [[1], [2], [3], [4]]
l2 = [['a'], ['b'], ['c']]

f :: [[b]] -> [[Char]]
f xs = [xs | y <- xs, xs <- l2, x <- xs]

intervalo :: Integer -> Integer -> [Integer]
intervalo m t
	| t < m = []
	| otherwise = m:(intervalo (m+1) t) 