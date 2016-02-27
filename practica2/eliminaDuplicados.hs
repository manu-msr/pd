-- Alfaro Mendoza Yoshua Ian, Patlani Aguilar Luis Angel
-- -----------------------------------
-- Programación Declarativa, 2016-2 --
-- Práctica 2                       --
--------------------------------------
import Data.Char

-- 1. elimDup
elimDup :: Eq a => [a] -> [a]
elimDup [] = []
elimDup l = foldr (\x (y:ys) -> if(x /= y) then (x:(y:ys)) else (y:ys)) [last l] l