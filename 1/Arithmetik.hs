--
-- ÜB 1, Programmierparadigmen 2014, KIT
--
-- Pascal Knodel
--
module Arithmetik where



import Test.QuickCheck




-- 1. Teil:
-----------



-- Aufgabe 1, "pow1":
---------------------


pow1 :: Integer -> Integer -> Integer
pow1 b e 
 
 |  e < 0      = error "Negativer Exponent"
 |  otherwise  = pow1' b e
 
 where
 
 pow1' _ 0 =  1
 pow1' b e =  b * pow1' b (e - 1)


-- Beispiel-Auswertung von "pow1 1 1":
--
--        pow1' 1 1
--       ?? 
--       ??      1 < 0
--       ??  ~>  False
--       ??
--       ??  otherwise
--       ??
--        pow1' 1 1
--       ??
--       ??      1 == 0
--       ??  ~>  False
--       ??
--       ??  otherwise
--       ??
--   ~>   1 * pow1' 1 (1 - 1)
--   ~>   1 * pow1' 1 0
--           ??
--           ??     0 == 0
--           ??  ~> True
--           ??
--   ~>   1 * 1
--   ~>   1


-- Beispiel-Auswertung von "pow1 1 2":
--
--        pow1' 1 2
--       ?? 
--       ??      2 < 0
--       ??  ~>  False
--       ??
--       ??  otherwise
--       ??
--        pow1' 1 2
--       ??
--       ??      2 == 0
--       ??  ~>  False
--       ??
--       ??  otherwise
--       ??
--   ~>   1 * pow1' 1 (2 - 1)
--   ~>   1 * pow1' 1 1
--           ??
--           ??     1 == 0
--           ??  ~> False
--           ??
--           ??  otherwise
--           ??
--   ~>   1 * 1 * pow1' 1 (1 - 1)
--   ~>   1 * 1 * pow1' 1 0
--   ~>   1 * 1 * 1
--
--
-- Anmerkung:
--
-- Da Haskell Ausdrücke, die für den aktuellen Schritt
-- nicht von Bedarf sind nicht auswertet steigt auch der Speicher-
-- verbrauch (Stapelspeicher / 'Stack') linear an.


prop_pow1 :: Integer -> (Positive Integer) -> Bool
prop_pow1 b (Positive e)

 =  pow1 b e == b ^ e

-- GHCi> quickCheck prop_pow1


-- Aufrufe von "pow1'":
--
-- "pow1'" ist durch Primitive Rekursion (Vergleiche: HASKELL, the craft of functional programming, 3. A., S. 84, A. 4.5)
-- definiert. Die Anzahl der Aufrufe von "pow1'" entspricht der Summe von
--
--       Anzahl der rekursiven Aufrufe von "pow1'"  =:  ? Aufrufe
--    +  Aufruf des Abbruchmusters von "pow'1"      =   1 Aufruf
--
-- "pow1'" ist rekursiv im zweiten Argument, dem Exponenten. In jedem Schritt wird der Exponent um 1 verringert,
-- bis 0 erreicht ist und das Abbruchmuster zutrifft.
--
--         Anzahl der rekursiven Aufrufe
--    <=>  Anzahl der Schritte bis das Argument für das Abbruchmuster erreicht ist
--    <=>  Anzahl der Subtraktionen (-1) vom Exponenten bis 0 erreicht ist
--    <=>  e - 'Summe von 1 bis a über 1'  =  0
--    <=>  e - a = 0
--    <=>  e = a
--    <=>  Der Algorithmus braucht e rekursive Aufrufe, bis das Abbruchargument erreicht ist.
-- 
-- Insgesamt sind es genau e + 1 Aufrufe von "pow1'".




-- Andere Lösungsvorschläge:



pow12 :: Integer -> Integer -> Integer
pow12 b e

 |  e < 0      = error "Negativer Exponent"
 |  e == 0     = 1
 |  otherwise  = b * pow12 b (e - 1)


-- Welche Probleme siehst Du bei der 'direkten Auswertung'
-- (ohne Optimierungen) von "pow1'" im Vergleich zu "pow1"?
--
--
-- Beispiel-Auswertung von "pow12 1 1":
--
--        pow12 1 1
--       ??
--       ??      1 < 0
--       ??  ~>  False
--       ??
--       ??      1 == 0
--       ??  ~>  False
--       ??
--       ??  otherwise
--       ??
--   ~>   1 * pow12 1 (1 - 1)
--   ~>   1 * pow12 1 0
--           ??
--           ??      0 < 0   -- Unnötige Muster-Überprüfung.
--           ??  ~>  False
--           ??
--           ??      0 == 0
--           ??  ~>  True
--           ??
--   ~>   1 * 1
--   ~>   1



pow13 :: Integer -> Integer -> Integer
pow13 b e

 |  e < 0      = error "Negativer Exponent"
 |  otherwise  = pow13' b e
 
 where
 
 pow13' :: Integer -> Integer -> Integer 
 pow13' _ 0 =  1
 pow13' b 1 =  b
 pow13' b e =  b * pow13' b (e - 1)


-- Welche Probleme siehst Du bei der 'direkten Auswertung'
-- (ohne Optimierungen) von "pow13'" im Vergleich zu "pow1"?
--
-- Beispiel-Auswertung von "pow13 1 2":
--
--        pow13 1 2
--       ??
--       ??      2 < 0
--       ??  ~>  False
--       ??
--       ??  otherwise
--       ??
--   ~>   pow13' 1 2
--       ??
--       ??      2 == 0
--       ??  ~>  False
--       ??  
--       ??      2 == 1   -- Im Allgemeinen wird hier zu oft unnötig überprüft.
--       ??               --
--       ??               -- (!)
--       ??               --
--       ??               -- Frage: Gibt es Szenarien in denen die Nutzung von Spezialfällen definitiv Sinn macht,
--       ??               -- z.B. bei Paralleler Programmierung?
--       ??  ~>  False
--       ??  
--       ??  otherwise
--       ??
--  ~>    1 * pow13' 1 (2 - 1)
--  ~>    1 * pow13' 1 1
--           ??
--           ??      1 == 0
--           ??  ~>  False
--           ??  
--           ??      1 == 1
--           ??  ~>  True
--           ??
--  ~>    1 * 1
--  ~>    1




-- Aufgabe 2, "pow2":
---------------------


pow2 :: Integer -> Integer -> Integer
pow2 b e

 |  e < 0      = error "Negativer Exponent"
 |  otherwise  = pow2' b e
 
 where
 
 pow2' :: Integer -> Integer -> Integer
 pow2' _ 0 =  1                                    -- Erstes Muster (Abbruch)
 pow2' b e                                         -- Zweites Muster
  
  |  even e     = pow2' (square b) (halve e)       -- Erster Fall
  |  otherwise  = b * pow2' (square b) (halve e)   -- Zweiter Fall
 
 square :: Integer -> Integer
 square b =  b * b
 
 halve :: Integer -> Integer
 halve e =  e `div` 2


 -- Beispiel-Auswertung von "pow2 1 4":
--
--        pow2 1 4
--       ??
--       ??      4 < 0
--       ??  ~>  False
--       ??
--       ??  otherwise
--       ??
--   ~>   pow2' 1 4                        -- 1. Aufruf
--       ??
--       ??      4 == 0
--       ??  ~>  False
--       ??
--       ??      even 4
--       ??  ~>  True
--       ??
--   ~>   pow2' (square 1) (halve 4)       --
--   ~>   pow2' (1 * 1) (4 `div` 2)        --
--   ~>   pow2' 1 2                        -- 2. Aufruf
--       ??
--       ??      2 == 0
--       ??  ~>  False
--       ??
--       ??      even 2
--       ??  ~>  True
--       ??
--   ~>   pow2' (square 1) (halve 2)       --
--   ~>   pow2' (1 * 1) (2 `div` 2)        --
--   ~>   pow2' 1 1                        -- 3. Aufruf 
--       ??
--       ??      1 == 0
--       ??  ~>  False
--       ??
--       ??      even 1
--       ??  ~>  False
--       ??
--       ??  otherwise
--       ??
--   ~>   1 * pow2' (square 1) (halve 1)   --
--   ~>   1 * pow2' (1 * 1) (1 `div` 2)    --
--   ~>   1 * pow2' 1 0                    -- 4. Aufruf
--           ??
--           ??     0 == 0
--           ??  ~> True
--           ??
--  ~>    1 * 1
--  ~>    1


-- Beispiel-Auswertung von "pow2 1 6":
--
--        pow2 1 6
--       ??
--       ??      6 < 0
--       ??  ~>  False
--       ??
--       ??  otherwise
--       ??
--   ~>   pow2' 1 6                                              -- 1. Aufruf
--       ??
--       ??      6 == 0
--       ??  ~>  False
--       ??
--       ??      even 6
--       ??  ~>  True                      -- Erster Fall
--       ??
--   ~>   pow2' (square 1) (halve 6)
--   ~>   pow2' (1 * 1) (6 `div` 2)
--   ~>   pow2' 1 3                                              -- 2. Aufruf
--       ??
--       ??      3 == 0
--       ??  ~>  False
--       ??
--       ??      even 3
--       ??  ~>  False
--       ??
--       ??  otherwise                     -- Zweiter Fall
--       ??
--   ~>   1 * pow2' (square 1) (halve 3)
--   ~>   1 * pow2' (1 * 1) (3 `div` 2)
--   ~>   1 * pow2' 1 1                                          -- 3. Aufruf
--           ??
--           ??      1 == 0
--           ??  ~>  False
--           ??
--           ??      even 1
--           ??  ~>  False
--           ??
--           ??  otherwise                 -- Zweiter Fall
--           ??
--   ~>   1 * 1 * pow2' (square 1) (halve 1)
--   ~>   1 * 1 * pow2' (1 * 1) (1 `div` 2)
--   ~>   1 * 1 * pow2' 1 0                                      -- 4. Aufruf
--   ~>   1 * 1 * 1
--   ~>   1 * 1
--   ~>   1


prop_pow2 :: Integer -> (Positive Integer) -> Bool
prop_pow2 b (Positive e)

 =  pow2 b e == b ^ e

-- GHCi> quickCheck prop_pow2


-- Aufrufe von "pow2'":
--
-- Die Anzahl der Aufrufe von "pow2'" entspricht der Summe von
--
--    +  Anzahl der rekursiven Aufrufe von "pow2'"   =:  ? Aufrufe
--    +  Aufruf des Abbruchmusters von "pow2'"       =   1 Aufruf
--
-- Unabhängig von der eingegebenen Zahl wird "pow2'" in jedem Fall und in jedem Schritt - außer dem letzten - ein Mal aufgerufen.
-- Die Änderung der Eingabe bleibt durch die jeweilige ganzzahlige Division durch 2 logarithmisch - als Baum:
--
--       Eingabe                    Aufrufe
--                    
--                    |              1
--                    0              
--                    |                 -|
--                    1                  |
--                    |                  |
--              |-----------|            |
--              2           3            |
--              |           |            |
--            |---|       |---|          |
--            4   5       6   7          |-  ilog 2 e + 1
--            |   |       |   |          |
--                                       |
--                   ...                 |
--                                       |
--       |     |                        -|
--       n   n + 1   ...
--
--
-- Es ist wichtig, dass bei der Definition das 'Quadrieren und Halbieren' auch in jedem Fall durch-
-- geführt wird (siehe Anmerkungen). Am Ende ist noch ein Rekursionsschritt im zweiten Muster von "pow2'"
-- auszuführen (siehe oben, Auswertungen "pow2 1 4" und "pow2 1 6", ab 2. Aufruf).
--
--          Anzahl der rekursiven Aufrufe
--    :<=>  Anzahl der Schritte bis das Argument für das Abbruchmuster erreicht ist
--    :<=>  Anzahl der Divisionen durch 2, bis der Exponent auf 1 reduziert ist (weil die letzte Division  2 `div` 2 = 1  ergibt)
--    :<=>  e `div` 'Produkt von 1 bis a über 2' = 1
--     <=>  e `div` 2^a = 1
--     <=>  e = 2^a
--     <=>  'Ganzzahliger Logarithmus zur Basis 2 von e' = a
--
-- Wir haben gezeigt, dass der nächste Schritt der Abbruchschritt ist.
-- Insgesamt sind es 'ilog 2 e + 1'  Aufrufe von "pow2'".
--
--
--
-- Anmerkungen:
--
--
-- Vorsicht!
--
-- Beim Übernehmen der mathematischen Definition, z.B. durch einen Zweiten Fall "|  otherwise  = b * pow2' b (e - 1)".
-- Das ist nicht mehr der gewünschte Algorithmus. Die Anzahl der Aufrufe ist im schlechtesten Fall höher, da nicht in
-- jedem Schritt quadriert und halbiert wird. Menschliche Fehler.
--
--
-- Die Teilbarkeit ist bei der Aufwandsanalyse interessant, da sich hier die Anzahl der Multiplikationen in den Fällen
-- 'Eingabe ist gerade' und 'Eingabe ist ungerade' unterscheiden. Im besten Fall ist die Eingabe eine Zweierpotenz und
-- bis auf den letzten rekursiven Aufruf trifft der erste Fall genau 'log 2 e' Mal zu.
--
-- Im schlechtesten Fall ist mehr zu untersuchen: Treten allgemein beide Fälle gleich oft ein?
-- Liegt der Aufwand in O('log 2 e')?
--
--
-- (!)
--
-- Frage: Ist die Aussage 'Da Haskell eine funktionale Sprache ist, entspricht
-- der Aufwand einer richtig übernomenen mathematische Definition dem der formalen Analyse'
-- richtig oder falsch? Warum (nicht)?



-- Wie viele Aufrufe braucht "pow2'" im Vergleich zu "pow1'"?
--
-- "pow1'":            e + 1 Aufrufe    O(e)
-- "pow2'":   'ilog 2 e' + 2 Aufrufe    O(ilog 2 e) = O(log 2 e)
--
-- Die Anzahl der Aufrufe bei "pow1'" ist linear, "pow2'" ist logarithmisch in (je in Abhängigkeit zur eingegebenen Zahl).
-- "pow2'" braucht - ab der Eingabe 3 - weniger Aufrufe.




-- Aufgabe 3, "pow3":
---------------------


-- Tipp:
--
-- Wenn die endrekursive Funktion 'in einem Stück' definiert werden soll,
-- wir das aber nicht können (zu viel auf einmal zu Denken fällt Menschen bekanntlich schwer),
-- ist es hilfreich, sich erst eine endrekursive Fallunterscheidung zu überlegen:


pow32 :: Integer -> Integer -> Integer
pow32 b e

 |  e < 0      = error "Negativer Exponent"
 |  otherwise  = pow32' b e 1
 
 where
 
 pow32' :: Integer -> Integer -> Integer -> Integer
 pow32' _ 0 a =  a
 pow32' b e a
  
  |  even e     = pow32' (square b) (halve e) a
  |  otherwise  = pow32' (square b) (halve e) (a * b)
 
 square :: Integer -> Integer
 square i =  i * i
 
 halve :: Integer -> Integer
 halve i =  i `div` 2


-- Möchte jemand die eigentliche Funktionsdefinition ohne Fallunterscheidung ('Guards') schreiben,
-- fällt das jetzt vielleicht einfacher:


pow3 :: Integer -> Integer -> Integer
pow3 b e

 |  e < 0      = error "Negativer Exponent"
 |  otherwise  = pow3' b e 1
 
 where
 
 pow3' :: Integer -> Integer -> Integer -> Integer
 pow3' _ 0 a =  a
 pow3' b e a =  pow3' (square b) (halve e) $! ( if (even e) then (a) else (a * b) )   -- Siehe Anmerkung zu "$!".
 
 square :: Integer -> Integer
 square i =  i * i
 
 halve :: Integer -> Integer
 halve i =  i `div` 2


-- Anmerkungen:
--
-- Wie/Warum geht das - Korrektheit?
--
-- Funktioniert, da wie bereits in "pow2'" ausgenutzt,
-- der vorletzte Schritt immer mit ungeradem Exponenten (e = 1) argumentiert.
-- D.h. der Wert der sich in "b" sammelt (genau genommen auch ein Akkumulator)
-- wird am Ende immer mit dem in "a" gesammelten Produkt multipliziert.
-- (Beweis: z.B. durch Strukturelle Induktion.)
--
--
-- "$!"? "pow3" ist noch nicht perfekt. Eine weitere Verbesserung ist die Anwendung
-- der 'strikten Argumentauswertung' auf den Akkumulator von "pow3'" mit sich.
-- "pow3' (square b') (halve e')" ist eine teilweise Anwendung von "pow3'"
-- (manchmal auch Unterversorgung genannt). Die entstehende Funktion erwartet
-- noch ein Argument, den Akkumulator. In Haskell gibt es eine Funktion, welche
-- ein Argument strikt (in ausgewerteter Form) übergibt: "$!".
-- Für "pow3'" sieht das dann so aus:
--
-- pow3' b e a =  pow3' (square b) (halve e) $! ( if (even e) then (a) else (a * b) )
--
-- Natürlich werden manche Übersetzungsprogramme diesen Fall auch ohne explizite
-- Anwendung von "$!" optimieren (weitere Informationen gibt z.B. die GHC Dokumentation,
-- Spezifikationen, ...).


prop_pow3 :: Integer -> (Positive Integer) -> Bool
prop_pow3 b (Positive e)

 =  pow3 b e == b ^ e

-- GHCi> quickCheck prop_pow3




-- Aufgabe 4, "root":
---------------------


root :: Integer -> Integer -> Integer
root e r

 |  e < 1      = error "Negativer Exponent"
 |  r < 0      = error "Negativer Radikant"
 |  otherwise  = rootBisection 0 (r + 1)
 
 where
 
 rootBisection low high
 
   | high - low == 1                = low
   | bisection `pow3` e <= (r + 1)  = rootBisection bisection high
   | otherwise                      = rootBisection low       bisection
   
  where
  
  bisection =  (low + high) `div` 2


-- Beispiel-Auswertung von "root 2 8":
--
--      root 2 8
--     ??
--     ??      2 < 1
--     ??  ~>  False
--     ??
--     ??      8 < 0
--     ??  ~>  False
--     ??
--     ??  otherwise
--     ??
--      rootBisection 0 9
--     ??
--     ??   where      bisection = (0 + 9) `div` 2
--     ??          ~>  9 `div` 2
--     ??          ~>  4
--     ??
--     ??      0 - 9 == 1
--     ??  ~>  9 == 1
--     ??  ~>  False
--     ??
--     ??      4 `pow3` 2 <= 9
--     ??  ~>  16 <= 9
--     ??  ~>  False
--     ??
--     ??  otherwise
--     ??
--      rootBisection 0 4
--     ??
--     ??   where      bisection = (0 + 4) `div` 2
--     ??          ~>  4 `div` 2
--     ??          ~>  2
--     ??
--     ??      0 - 4 == 1
--     ??  ~>  4 == 1
--     ??  ~>  False
--     ??
--     ??      2 `pow3` 2 <= 9
--     ??  ~>  4 <= 9
--     ??  ~>  True
--     ??
--      rootBisection 2 4
--     ??
--     ??   where      bisection = (2 + 4) `div` 2
--     ??          ~>  6 `div` 2
--     ??          ~>  3
--     ??
--     ??      4 - 2 == 1
--     ??  ~>  2 == 1
--     ??  ~>  False
--     ??
--     ??      3 `pow3` 2 <= 9
--     ??  ~>  9 <= 9
--     ??  ~>  True
--     ??
--      rootBisection 3 4
--     ??
--     ??   where      bisection = (3 + 4) `div` 2
--     ??          ~>  7 `div` 2
--     ??          ~>  3
--     ??
--     ??      4 - 3 == 1
--     ??  ~>  1 == 1
--     ??  ~>  True
--     ??
--      3


{- GHCi>

root 2 8

-}
-- 3


prop_root :: (Positive Integer) -> (Positive Integer) -> Property
prop_root (Positive e) (Positive r)

 =       e > 1
    ==>  (root e r) `elem` [x - 2 , x - 1 , x , x + 1 , x + 2]   -- 

 where
 
 eFloat = fromInteger e :: Float
 rFloat = fromInteger r :: Float
 
 x = negate $ round ( log rFloat / (log $ 1 / eFloat) )

-- GHCi> verboseCheck prop_root

-- Warnung: (Ganzzahlen gegen Gleitkommazahlen).
--
{- GHCi> quickCheck prop_root

*** Failed! Falsifiable (after 87 tests and 1 shrink):
Positive {getPositive = 2}
Positive {getPositive = 80}

-}
-- GHCi> root 2 80
-- 9
-- GHCi> log 80 / log (1/2)
-- -6.321928094887362
-- GHCi> round it
-- -6
-- GHCi> negate it
-- 6
--
-- Jedoch gibt er einen ersten Eindruck, ob die Umsetung der Definition überhaupt funktioniert.




-- Aufgabe 5, "isPrime":
------------------------


isPrime :: Integer -> Bool
isPrime n

 |  n < 2      = error "Kleiner Zwei"
 |  otherwise  = null [ 1 | d <- 2 : [ 3, 5 .. root 2 n ] , n `mod` d == 0 ]


{- GHCi>

isPrime 123457

-}
-- True




