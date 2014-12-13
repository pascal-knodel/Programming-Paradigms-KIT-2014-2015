--
-- ÜB 4, Programmierparadigmen KIT 2014/2015
--
-- Pascal Knodel
--
module Types where




-- Geben Sie für folgende Haskell-Funktionen die allgemeinstmöglichen Typen 
-- einschließlich etwaiger Typklasseneinschränkungen an. Begründen Sie, warum
-- diese Einschränkungen nötig sind.



fun1 xs =  (xs == [])

-- Von innen nach außen:
--
-- Aus dem Vergleich von xs mit einer leeren Liste []
-- folgt, dass auch xs ein Liste beliebigen Typs, auf
-- dem eine Vergleichsrelation definiert ist, sein muss.
--
-- Der Typ beginnt mit: (Eq t) => [t] -> ... 
-- 
-- Dann ist schon das Äußerste erreicht, der Vergleich resultiert in 
-- einem boolschen Wert.
--
-- Der allgemeinstmögliche Typ lautet: (Eq t) => [t] -> Bool

-- GHCi> :t fun1
-- fun1 :: Eq t => [t] -> Bool



fun2 f a =  foldr f "a"

-- Erinnerung: 
--
--
--
--

-- GHCi> :t fun2
--



fun3 f a xs c =  foldl f a xs c

--
--

-- GHCi> :t fun3
--



fun4 f xs =  map f xs xs

--
--

-- GHCi> :t fun4
--



fun5 a b c =  (maximum [a..b], 3 * c)

--
--

-- GHCi> :t fun5
--



fun6 x y =  succ (toEnum (last [fromEnum x .. fromEnum y]))

--
--

-- GHCi> :t fun6
--



fun7 x =  if show x /= [] then x else error

--
--

-- GHCi> :t fun7
--




