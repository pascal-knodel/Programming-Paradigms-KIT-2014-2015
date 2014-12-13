--
-- ÜB 3, Programmierparadigmen KIT 2014/2015
--
-- Pascal Knodel
--
module Intersect where




-- Teil 3:
----------


-- Zuerst Zusicherungen notieren und später ausnutzen:
--
-- (!) Sortierte Listen.
-- (!) Keine Duplikate.



-- Aufgabe 1:
-------------


-- (!) Schnitt zweier Mengen namens M1 und M2:
--     'M1 geschnitten M2' = 'alle m, die in M1 und M2 sind'
--
-- (!) "(Ord t)", "Ord" kennen und verstehen.
--
-- (!) Streams dürfen für die Funktion kein Problem sein.


intersect :: (Ord t) => [t] -> [t] -> [t]
intersect as [] = []
intersect [] bs = []
intersect (a : as) (b : bs)

 |  a == b     = a : intersect      as       bs    -- 'Bedingung vom Schnitt' erfüllt, ein Element (a oder b) wählen und damit die unendliche Liste konstruieren.
 |  a >  b     =     intersect (a : as)      bs    -- Wegen den Zusicherungen sind alle folgenden bs größer. Wir können b ignoriern.
 |  otherwise  =     intersect      as  (b : bs)   -- a ignoriern.




-- Was wäre keine Lösung?

nosi :: (Ord t) => [t] -> [t] -> [t]
nosi _ [] = []
nosi [] _ = []
nosi (a : as) bs

 |  a `elem` bs  = a : nosi as bs
 |  otherwise    =     nosi as bs


-- nosi; 'no stream intersect' ...
--
-- ... erzeugt keine 'schrittweise' / 'fließend' unendliche Liste, wenn beide Eingabelisten unendlich sind.
-- ... terminiert nicht, wenn die erste Liste unendlich ist.
--
-- GHCi> nosi [1 ..] [1,2]
-- [1,2
-- Nichts passiert.
-- STRG + C (Windows CMD)
-- [1,2Interrupted




-- Aufgabe 2:
-------------


intersectAll :: (Ord t) => [[t]] -> [t]
intersectAll [a] = a
intersectAll (a : as)

 =  a `intersect` intersectAll as

intersectAll []  = error "Leere Liste von Listen."

-- GHCi> intersectAll [ [1 ..] , [1 .. 10] ]
-- [1,2,3,4,5,6,7,8,9,10]

-- GHCi> intersectAll [ [1 .. 10] , [1 ..] ]
-- [1,2,3,4,5,6,7,8,9,10]

-- GHCi> intersectAll [ [1 ..] , [1] , [1 ..] ]
-- [1]

-- GHCi> intersectAll [ [1 ..] , [1 ..] ]
-- 
-- Produziert den Schnitt-'stream'.



intersectAll2 :: (Ord t) => [[t]] -> [t]
intersectAll2 []  = undefined
intersectAll2 (a : as) =  foldr intersect a as




-- Aufgabe 3:
-------------


commonMultiples :: Integer -> Integer -> Integer -> [Integer]
commonMultiples i1 i2 i3

 =  let multiples i = [ i * f | f <- [1 ..] ]
 
    in  intersectAll [  multiples i  |  i <- [i1 , i2 , i3]  ]

-- GHCi> commonMultiples 1 1 1
--
-- Produziert den 'stream' der natürlichen Zahlen ohne 0.



commonMultiples2 :: Integer -> Integer -> Integer -> [Integer]
commonMultiples2 i1 i2 i3

 =  intersectAll [ multiples i | i <- [i1 , i2 , i3] ]
 
 where
 
 multiples :: Integer -> [Integer]
 multiples i =  [ i * f | f <- [1 ..] ]




-- Erfahrungen aus dieser Aufgabe / Klausur Tipps:
-- 
-- (!) Mit mehreren 'list comprehensions' arbeiten,
--     nicht alles in eine packen (wollen).
--
-- (!) Bei Listen von Listen auf die richtige Umklammerung achten.




