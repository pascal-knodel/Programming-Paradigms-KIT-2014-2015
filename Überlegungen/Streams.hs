--
-- Überlegungen / Eigene Übungsaufgaben zu Streams.
--
module Streams where



-- 1.
-----


-- Intervallhalbierungsverfahren.
--
-- Eingaben:
--
-- "f", eine monotone, stetige Funktion.
--
-- "(a , b)", zwei Werte auf der horizontalen Achse "(a , b)" := [a , b],
-- zwischen denen eine Nullstelle liegt.
--
--
-- Vereinfachung: Dieses Beispiel soll sich nicht um die Probleme bei Arithmetik
-- mit Zahlen fester/eingeschränkter Genauigkeit, zu denen "Double"s gehören kümmern.


rootas :: (Double -> Double) -> (Double , Double) -> [Double]   -- root approximation stream
rootas function (low , high)

 |  signum (function low) /= signum (function section)  = section : rootas function (low , section)
 |  otherwise                                           = section : rootas function (section , high)
 
 where
 
 section =  (low + high) / 2
 

-- Beispiele:

p :: Double -> Double
p x =  x^2 - 2   -- NST ist  sqrt(2) = 1.414 ...  .

-- GHCi> vlo $ rootas p (1 , 2)
-- 1.5
-- 1.25
-- 1.375
-- 1.4375
-- 1.40625
-- 1.421875
-- 1.4140625
-- 1.41796875
-- 1.416015625
-- 1.4150390625
-- 1.41455078125
-- 1.414306640625
-- 1.4141845703125
-- 1.41424560546875
-- 1.414215087890625
-- 1.4141998291015625
-- 1.4142074584960938
-- 1.4142112731933594
-- 1.4142131805419922
-- 1.4142141342163086
-- 1.4142136573791504
-- 1.4142134189605713
-- 1.4142135381698608
-- 1.4142135977745056
-- ...


-- Hilfsfunktion:

vlo :: Show a => [a] -> IO ()   -- "vlo", vertical list output
vlo [] =  putStr $ ""
vlo (a : as)

 =  do putStr $ show a ++ "\n"
       vlo as




-- 2.
-----


-- Harmonische Reihe (Folge von Partialsummen).
--
-- Partialsummen der harmonischen Reihe in einer unendlichen Liste.


-- Quick And Dirty:   [  sum [ 1 / d | d <- [1 .. n] ]  |  n <- [1 ..]  ]


hns :: [Double]   -- harmonic numbers stream
hns

 =  hns' 1 0
 
 where
 
 hns' :: Double -> Double -> [Double]
 hns' i a
 
  =  (a + 1 / i) : hns' (i + 1) (a + 1 / i)


-- GHCi> (hns !! 3) - (hns !! 1)
-- 0.583333333333333



hsdas :: [Double]   -- harmonic series difference assumption stream
hsdas

 =  [ hns !! (2 * n - 1) - hns !! (n - 1) | n <- [1 ..] ]



-- Hilfsfunktion:

vloi :: Show a => Integer -> [a] -> IO ()   -- "vloi", vertical list output (with) index
vloi i [] =  putStr $ ""
vloi i (a : as)

 =  do putStr $ show i ++ ": " ++ show a ++ "\n"
       vloi (i + 1) as




