module Conjuntos where

data Set a = ESet | CSet a (Set a) deriving (Read, Show)

listToSet :: [a] -> Set a
listToSet [] = ESet
listToSet (x:xs) = CSet x (listToSet xs)

setToList :: Set a -> [a]
setToList ESet = []
setToList (CSet a c) = [a] ++ setToList c

isEmpty :: Set a -> Bool
isEmpty ESet = True
isEmpty (CSet a c) = False

--inSet :: a -> Set a -> Bool

addtoSet :: a -> Set a -> Set a
addtoSet a ESet = (CSet a ESet)
addtoSet a c = (CSet a c)