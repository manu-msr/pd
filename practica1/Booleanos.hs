-- -----------------------------------
-- Programación Declarativa, 2016-2 --
-- Práctica 2                       --
-- Sección 1: Booleanos             --
--                                  --
-- Integrantes:                     --
-- 1. Alfaro Mendoza Yoshua Ian     --
-- 2. Patlani Aguilar Luis Ángel    --
-- 3. Soto Romero Manuel            --
--------------------------------------
module Booleanos where

----------------------------------------------------------------------------------
-- 1. Las operaciones booleanas posibles entre dos valores booleanos son 16. En -- 
-- el preludio de HASKELL están definidas: && (and), || (or) y not. Define el   --
-- resto de las operaciones.                                                    --
----------------------------------------------------------------------------------

-- 1. Not
not' :: Bool -> Bool
not' True = False
not' False = True

-- 2. And
and' :: Bool -> Bool -> Bool
and' True True = True
and' False _ = False
and' _ False = False

-- 3. Or
or' :: Bool -> Bool -> Bool
or' False False = False
or' True _ = True
or' _ True = True

-- 4. Constante binaria 0
cons0 :: Bool -> Bool -> Bool
cons0 _ _ = False

-- 5. Inhibición
inh1 :: Bool -> Bool -> Bool
inh1 a b = and' a (not' b)

-- 6. Transferencia
trans1 :: Bool -> Bool -> Bool
trans1 a _ = a

-- 7. Inhibición
inh2 :: Bool -> Bool -> Bool
inh2 a b = and' b (not' a)

-- 8. Transferencia
trans2 :: Bool -> Bool -> Bool
trans2 _ b = b

-- 9. Or exclusivo
orexc :: Bool -> Bool -> Bool
orexc a b = (or' (inh1 a b) (inh2 a b))

-- 10. Nor
nor :: Bool -> Bool -> Bool
nor a b = (not' (or' a b))

-- 11. Equivalencia
equiv :: Bool -> Bool -> Bool
equiv a b = (or' (and' a b) (and' (not' a) (not' b)))

-- 12. Complemento
comp1 :: Bool -> Bool -> Bool
comp1 _ b = not' b

-- 13. Implicación
imp1 :: Bool -> Bool -> Bool
imp1 a b = (or' a (not' b))

-- 14. Complemento
comp2 :: Bool -> Bool -> Bool
comp2 a _ = not' a

-- 15. Implicación
imp2 :: Bool -> Bool -> Bool
imp2 a b = (or' (not' a) b)

-- 16. Nand
nand :: Bool -> Bool -> Bool
nand a b = (not' (and' a b))

-- 17. Identidad
idn :: Bool -> Bool -> Bool
idn _ _ = True