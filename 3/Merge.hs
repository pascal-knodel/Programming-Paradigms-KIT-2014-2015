--
-- ÜB 3, Programmierparadigmen KIT 2014/2015
--
-- Pascal Knodel
--
module Merge where




-- Teil 2:
----------


-- Zuerst Zusicherungen notieren und später ausnutzen:
--
-- (!) Sortierte Listen.



-- Aufgabe 1:
-------------


merge :: Ord t => [t] -> [t] -> [t]
merge (a : as) (b : bs)
 
 |  a <= b     = a : merge      as  (b : bs)
 |  otherwise  = b : merge (a : as)      bs

merge [] bs =  bs
merge as [] =  as


-- Achtung:
--
-- (!) Unterschiedliche Listenlänge beachten.
--
-- (!) Es können keine zwei Elemente a b auf einmal herausgenommen werden,
--     da nachdem ein Element (entweder a oder b) vorne einsortiert wurde,
--     könnte das folgende Element a2 aus (a1 : a2 : as) auch vor b liegen.
--     Deshalb immer nur ein Element voranstellen.



wrongMerge :: Ord t => [t] -> [t] -> [t]
wrongMerge [] bs =  bs
wrongMerge as [] =  as
wrongMerge (a : as) (b : bs)
 
 |  a <= b     = a : b : wrongMerge as bs
 |  otherwise  = b : a : wrongMerge as bs

-- GHCi> wrongMerge [1,1] [2]
-- [ 1 , 2 , 1 ]



-- Alternative Definition von "merge":

merge2 :: Ord t => [t] -> [t] -> [t]
merge2 (a : as) (b : bs)
 
 |  a <= b     = a : merge2      as (b : bs)
 |  otherwise  = b : merge2 (a : as)     bs

merge2 as bs =  as ++ bs   -- 'catch all'




-- Aufgabe 2:
-------------


primes :: [Integer]
primes

 =  sieve [2..]
 
 where
 
 sieve [] = []
 sieve (p:xs) = p : sieve (filter (not . multipleOf p) xs)
 
 multipleOf p x = x `mod` p == 0



primepowers :: Integer -> [Integer]
primepowers n

 =  primepowers' powerstreams
 
 where
 
 powerstreams :: [[Integer]]
 powerstreams = [ map (^e) primes | e <- [1..n] ]
 
 primepowers' :: [[Integer]] -> [Integer]
 primepowers' [] = []
 primepowers' (p : ps) =  p `merge` primepowers' ps
 
 

primepowers2 :: Integer -> [Integer]
primepowers2 n =  foldr merge [] [ map (^e) primes | e <- [1..n] ]



primepowers3 :: Integer -> [Integer]
primepowers3 n =  foldr1 merge [ map (^e) primes | e <- [1..n] ]




