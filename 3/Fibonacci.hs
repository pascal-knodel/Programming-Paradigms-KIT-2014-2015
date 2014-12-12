--
-- ÜB 3, Programmierparadigmen KIT 2014/2015
--
-- Pascal Knodel
--
module Fibonacci where



-- Was sind Streams?
--
-- * I. E.: Unendliche Listen.
--
-- Fragen: (1) Was ist bei Funktionsdefinitionen über Streams zu beachten?
--         (2) Was ist alles als Stream definierbar?
--         (3) Was sollte als Stream definiert werden - was nicht?
--         (4) Welche Vorteile haben Streams?




-- Teil 1:
----------



-- Einfache Möglichkeit, primitive Rekursion:
---------------------------------------------


fibs :: [Integer]
fibs
 
 =  0 : 1 : fibs' 0 1
 
 where
 
 fibs' :: Integer -> Integer -> [Integer]
 fibs' a b =  (a + b) : fibs' b (a + b)



fibs2 :: [Integer]
fibs2

 =  0 : fibs2' 1 1
 
 where
 
 fibs2' :: Integer -> Integer -> [Integer]
 fibs2' a b = a : fibs2' b (a + b)




-- Mit der Verwendung von "zipWith":
------------------------------------


-- Hilfe: Rekursionsstrategien lassen sich erraten, werden die Berechnungen
--        einiger Glieder der Fibonacci Folge 'lazy' nacheinander hingeschrieben,
--        d.h. keine Addition durchführen und nur die 0en und 1en betrachtet
--        (evtl. umsortieren).


fibz :: [Integer]
fibz

 =  0 : 1 : zipWith (+) fibz (tail fibz)


-- zipWith :: (a->b->c) -> [a]->[b]->[c]
-- zipWith f (a:as) (b:bs) =  f a b : zipWith f as bs
-- zipWith _ _      _      =  []


--       fibz
--   ~>  0 : 1 : zipWith fibz (tail fibz)
--   ~>  0 : 1 : zipWith (0 : 1 : zipWith fibz (tail fibz)) (tail (0 : 1 : zipWith fibz (tail fibz)))
--   ~>  0 : 1 : (0 + 1) : zipWith (1 : zipWith fibz (tail fibz)) (1 : zipWith fibz (tail fibz))
--   ~>  0 : 1 : (0 + 1) : (1 + 1) zipWith (zipWith fibz (tail fibz)) (zipWith fibz (tail fibz))
--   ~>  0 : 1 : (0 + 1) : (1 + 1) zipWith (zipWith (zipWith fibz (tail fibz)) (tail (zipWith fibz (tail fibz)))) (zipWith (zipWith fibz (tail fibz)) (tail (zipWith fibz (tail fibz))))
--   ...




-- Beispiel für "fib"-Funktion, die 'funktionieren', jedoch mehrere Mängel haben:
---------------------------------------------------------------------------------


badFibs :: [Integer]
badFibs

 =  0 : 1 : badFibs' 1 [1 ..]
 
 where
 
 badFibs' n a =  (a !! 0) : badFibs' (a !! 0) ( drop (fromInteger n :: Int) a )


-- Diese Funktion liefert keine unendliche Liste - Warum?
-- (Hinweis: Es liegt nicht nur am Int)
-- 
-- drop (fromInteger n :: Int) a
--      ----------------------
--
--
-- Fragen: Hilft die Verwendung eines Musters der Art "fib2' (2^29 - 1) _ = ..."
--         Probleme mit beschränkten Int's zu behandeln? Wie wird der Fall am besten behandelt?
--         Was sollte man auf keinen Fall tun? (error , Maybe, ...)?
--
--
-- Die Funktion "drop" geht jedes Mal eine mit der 'Fibonacci Differenz' länger
-- werdende Liste durch. Es wird bei jedem Aufruf mehr Speicher und Zeit benötigt
-- (schlechterer, verschwenderischer Algorithmus im Gegensatz zu den anderen gezeigten Definitionen).
-- Der Bedarf wächst dabei <Wort für die Art des Wachstums einsetzen> (differenz-fibonaccial?).

-- GHCi> badFibs
--
-- Bei mir stürzt bei diesem Aufruf GHCi nach ca. 3 Standard-Windows-8.1-CMD-Zeilen ab,
-- viel eher, als dass der Int Wertebereich überschritten wird (10.12.2014).
--
-- GHCi> badFibs !! 50
--
-- Genauso.
--
-- GHCi> badFibs !! 20
-- 6765



badFibs2 :: [Integer]
badFibs2

 =  [ fib n | n <- [1 ..] ]
 
 where
 
 fib :: Integer -> Integer
 fib 0 = 0
 fib 1 = 1
 fib n = fib (n - 1) + fib (n - 2)

-- GHCi> badFibs2
-- 
-- Anzahl der rekursiven Aufrufe wächst dabei <Wort für die Art des Wachstums einsetzen> (fibonaccial?).




-- Erfahrungen aus dieser Aufgabe / Klausur Tipps:
--
-- (!) Probe machen. Wenn noch Zeit ist, ein paar Aufrufe
--     von Funktionen wie dieser im Kopf oder schriftlich auswerten.




