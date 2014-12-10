--
-- ÜB 1, Programmierparadigmen 2014, KIT
--
-- Pascal Knodel
--
module Sort where



import Test.QuickCheck

import qualified Data.List
 (
   insert
 , sort
 )




-- 2. Teil:
-----------



-- "insert":
------------


-- Definition durch Primitive Rekursion:
--
--   integer               -  Ganzzahl.
--   listInteger           -  Erste Ganzzahl der sortierten Liste.
--   remainingIntegerList  -  Restliste ganzer Zahlen.


insert :: Integer -> [Integer] -> [Integer]
insert integer [] =  [integer]
insert integer (listInteger : remainingIntegerList)

 |  integer <= listInteger  = integer : listInteger : remainingIntegerList
 |  otherwise               = listInteger : insert integer remainingIntegerList


{- GHCi>

insert 1 [0, 2]

-}
-- [ 0 , 1 , 2 ]


prop_insert :: Integer -> [Integer] -> Bool
prop_insert integer integerList

 =  insert integer sortedIntegers == Data.List.insert integer sortedIntegers
 
 where
 
 sortedIntegers :: [Integer]
 sortedIntegers = Data.List.sort integerList

-- GHCi> quickCheck prop_insert




-- Definition von "insert" über vorhandene Funktionen:


insert2 :: Integer -> [Integer] -> [Integer]
insert2 integer integerList

 =  lesser ++ (integer : greater)
 
 where
 
 -- (lesser , greater) :: (Integer , Integer)
 (lesser , greater) =  break (>= integer) integerList


-- Nachteile?
--
--    Aufwand(insert2) = Aufwand(++) + Aufwand(break) > Aufwand(insert) <=> Aufwand('Primitive Rekursion')



prop_insert2 :: Integer -> [Integer] -> Bool
prop_insert2 integer integerList

 =  insert2 integer sortedIntegers == Data.List.insert integer sortedIntegers
 
 where
 
 sortedIntegers :: [Integer]
 sortedIntegers = Data.List.sort integerList

-- GHCi> quickCheck prop_insert2



-- "insertSort":
----------------


insertSort :: [Integer] -> [Integer]
insertSort [] =  []
insertSort (integer : remaining)

 =  insert integer $ insertSort remaining


prop_insertSort :: [Integer] -> Bool
prop_insertSort integerList

 =  insertSort integerList == Data.List.sort integerList

-- GHCi> quickCheck prop_insertSort



-- Definition von "insertSort" über Kombinator mit Unterversorgung:


insertSort2 :: [Integer] -> [Integer]
insertSort2

 =  foldr insert []


-- Hinweis:
--
-- GHCi> insertSort2 [0, -1 ..]
--
-- Was wird passieren?
--
--    (A) Unendlich lange nichts.
--    (B) Etwas Anderes.
--
--
{- GHCi>

insertSort2 [0, -1 ..]

-}
-- *** Exception: stack overflow




-- 3. Teil:
-----------



-- "merge":
-----------


merge :: [Integer] -> [Integer] -> [Integer]
merge [] gs =  gs
merge ls [] =  ls
merge (l : ls) (g : gs)

 |  l < g      =  l : merge      ls (g : gs)
 |  otherwise  =  g : merge (l : ls)     gs


{- GHCi>

merge [0] [2, 1]

-}
-- [ 0 , 1 , 2 ]
 



-- "mergeSort":
----------------
 

mergeSort :: [Integer] -> [Integer]
mergeSort integerList

 |  aboutHalf /= 0  = merge (mergeSort left) (mergeSort right)
 |  otherwise       = merge integerList []

 where
 
 aboutHalf :: Int
 aboutHalf = length integerList `div` 2
 
 (left , right) =  seperateAt aboutHalf integerList


seperateAt :: Int -> [Integer] -> ([Integer], [Integer])
seperateAt index (integer : integerList)
 
 |  index > 0  = let (left , right) = seperateAt (index - 1) integerList
                 in  (integer : left , right)
 |  otherwise  = ([] , integer : integerList)
 
 
{- GHCi>

seperateAt [1 , 2 , 3 , 4] 2

-}
-- 


prop_mergeSort :: [Integer] -> Bool
prop_mergeSort integerList

 =  mergeSort integerList == Data.List.sort integerList

-- GHCi> quickCheck prop_mergeSort



mergeSort2 :: [Integer] -> [Integer]
mergeSort2 integerList

 |  half /= 0  = merge (mergeSort2 left) (mergeSort2 right)
 |  otherwise  = merge integerList []

 where
 
 half :: Int
 half = length integerList `div` 2
 
 (left , right) = (take half integerList, drop half integerList)




