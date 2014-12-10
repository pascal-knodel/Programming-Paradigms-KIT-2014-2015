module Polynom where




-- Teil 2: Listenkombinatoren
-----------------------------


type Polynom =  [Double]



-- Aufgabe 1:
-------------


add :: Polynom -> Polynom -> Polynom
add (a : as) (b : bs) =  (a + b) : add as bs
add as bs =  as ++ bs


add' :: Polynom -> Polynom -> Polynom
add' as [] = as
add' [] bs = bs
add' (a : as) (b : bs) =  (a + b) : add as bs




-- Aufgabe 2:
-------------


eval :: Polynom -> Double -> Double
eval as x =  foldr (\a1 a2 -> a1 + x * a2) 0 as


eval' :: Polynom -> Double -> Double
eval' as x =  (    foldl (  \g a1 a2 -> g ( (\a1 a2 -> a1 + x * a2) a1 a2 )  ) id as    ) 0


-- foldl :: (b -> a -> b) -> b -> [a] -> b
-- foldl f z0 xs0
--
--  = lgo z0 xs0
--
--  where
--
--  lgo z []     =  z
--  lgo z (x:xs) =  lgo (f z x) xs


-- Beispiel-Auswertung:
--
--      eval' as@[ 1 ] x@2
--  ~>  ( foldl f@( \g a1 a2 -> g ( (\a1 a2 -> a1 + 2 * a2) a1 a2 ) ) z0@id xs0@[ 1 ] ) 0
--  ~>  ( lgo z0@id xs0@[ 1 ] ) 0
--
--  ~>  ( lgo z@id (x:xs)@( 1 : [] ) ) 0
--  ~>  ( lgo (f z x)@( ( \g a1 a2 -> g ( (\a1 a2 -> a1 + 2 * a2) a1 a2 ) ) id 1 ) xs@[] ) 0
--
--  ~>  ( lgo z@( ( \g a1 a2 -> g ( (\a1 a2 -> a1 + 2 * a2) a1 a2 ) ) id 1 ) [] ) 0
--  ~>  ( ( \g a1 a2 -> g ( (\a1 a2 -> a1 + 2 * a2) a1 a2 ) ) id 1 ) 0
--
--  ~>  ( ( \a2 -> id ( (\a1 a2 -> a1 + 2 * a2) 1 a2 ) ) id 1 ) 0
--  ~>  ( \a2 -> id ( (\a1 a2 -> a1 + 2 * a2) 1 a2 ) ) 0
--  ~>  id ( (\a1 a2 -> a1 + 2 * a2) 1 0 )
--  ~>  id ( 1 + 2 * 0 )
--  ~>  id ( 1 )
--  ~>  1


-- Beschreibung: Rekursion mit bei jedem Rekursionsschritt unterversorgter Funktion.


-- GHCi> eval' [1] 2
-- 1.0




-- Aufgabe 3:
-------------


deriv :: Polynom -> Polynom
deriv as = zipWith (*) [1 ..] $ tail as




-- GHCi-Beispiele:
------------------
--
{- GHCi>

let as = [0 , 1]
let bs = [0 , 1 , 2]

-}
-- GHCi> add as bs
-- [ 0.0 , 2.0 , 2.0 ]
--
-- GHCi> add' as bs
-- [ 0.0 , 2.0 , 2.0 ]
--
-- GHCi> eval as 2
-- 2.0
--
-- GHCi> eval bs 2
-- 10.0
--
-- GHCi> eval' as 2
-- 2.0
--
-- GHCi> eval' bs 2
-- 10.0
--
-- GHCi> deriv as
-- [ 1.0 ]
--
-- GHCi> deriv bs
-- [ 1.0 , 4.0 ]




-- Achtung:
--
-- (!) Allgemeine Rekursions-Kombinatoren kennen
-- (!) Listenkombinatoren kennen
-- (!) Bei den Definitionen aufpassen
--     * Was ist der letzte Wert (z.B. bei 'folds')?
--     * Werden Elemente einer Liste entfernt oder ausgelassen?
--     * Werden Elemente einer Liste hinzugefügt?
--     * Funktionen höherer Ordnung mit Lambda Abstraktionen als Argument
--     * Lambda Abstraktionen als anonyme Funktionen höherer Ordnung
--     * ...




