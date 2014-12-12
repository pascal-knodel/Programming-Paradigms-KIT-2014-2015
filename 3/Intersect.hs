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
-- ... erzeugt keine 'schrittweise' unendliche Liste, wenn beide Eingabelisten unendlich sind.
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
intersectAll =  undefined




-- Aufgabe 3:
-------------


commonMultiples a b c =  undefined




