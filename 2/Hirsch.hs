--
-- ÜB 2, Programmierparadigmen KIT 2014/2015
--
-- Pascal Knodel
--
module Hirsch where



-- import Prelude ( reverse )
import Data.List ( sort )

import Data.List ( findIndex )
-- In alternativer Def. "hIndex'" verwendet.




hIndex :: [Int] -> Int
hIndex pis

 =  hIndex' (reverse . sort $ pis) 0
 
 where
 
 hIndex' :: [Int] -> Int -> Int
 hIndex' [] i =  i
 hIndex' (pi : pis) i
  
  |  pi > i     = hIndex' pis (i + 1)
  |  otherwise  = i


-- Achtung:
--
-- (!) Richtig zählen.
-- (!) Keine Argumente vergessen.
-- (!) Rekursion nicht mit gleicher Liste durchführen (sie sollte das Problem
--     ab irgend einem Schritt vereinfachen und schließlich lösen).



{- GHCi>

hIndex [ 1 , 2 , 3 , 4 , 5  ,6 ]

-}
-- 4




hIndex' :: [Int] -> Int
hIndex' =  ( \(Just i) -> i ) . findIndex ( \(i , n) -> n < i ) . zip [ 1 .. ] . reverse . sort




