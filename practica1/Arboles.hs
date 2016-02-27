-- -----------------------------------------
-- Programación Declarativa, 2016-2       --
-- Práctica 2                             --
-- Sección 6: Arboles Binarios            --
--                                        --
-- Integrantes:                           --
-- 1. Alfaro Mendoza Yoshua Ian           --
-- 2. Patlani Aguilar Luis Ángel          --
-- 3. Soto Romero Manuel                  --
--------------------------------------------
module Arboles where

data BTree a = Leaf a | BT (BTree a) (BTree a) deriving (Read, Show)
--------------------------------------------------------------------
-- 1. Funciones de un arbol binario con informacion en las hojas: --
--------------------------------------------------------------------

-- addleaf
addleaf :: BTree a -> a -> BTree a
addleaf (Leaf l)  v = (BT (Leaf l) (Leaf v))
addleaf b v = (BT b (Leaf v))

-- leafs
leafs :: BTree a -> Int
leafs (Leaf a) = 1
leafs (BT l r) = (leafs l) + (leafs r)

-- depth
depth :: BTree a -> Int
depth (Leaf a) = 0
depth (BT l r) = 1 + (maxD (depth l) (depth r))

-- Funcion auxiliar
maxD :: Int -> Int -> Int
maxD a b
	|a > b = a
	|otherwise = b

-- size
size :: BTree a -> Int
size (Leaf a) = 1
size (BT l r) = 1 + (size l) + (size r)

-- map
mapBT :: (a -> b) -> BTree a -> BTree b
mapBT f (Leaf a) = (Leaf (f a))
mapBT f (BT l r) = (BT (mapBT f l) (mapBT f r))