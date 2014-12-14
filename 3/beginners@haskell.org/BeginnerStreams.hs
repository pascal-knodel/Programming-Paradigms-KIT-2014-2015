module BeginnerStreams where



-- Define a function that takes two lists, both of which have elements
-- in growing order (1, 2, 3, 3, 5, 9, 9, ...), and outputs a list with
-- the type of [Either a a], where a `Left a` would represent an element
-- from the first list, a `Right a` from the second list. Make it so that
-- the elements are in order, and also that the lefts preceede the rights.
-- 
-- Example, the inputs [1, 1, 3, 7, 7] and [1, 2, 3, 5, 5] would result in
--
-- [ Left  1 ,
--   Left  1 ,
--   Right 1 ,
--   Right 2 ,
--   Left  3 ,
--   Right 3 ,
--   Right 5 ,
--   Right 5 ,
--   Left  7 ,
--   Left  7 ]
--
--
-- (beginners@haskell.org, Re: [Haskell-beginners] exercises/examples for streams))



-- 1. First try:


-- Contract:   input lists are sorted.
--
-- Notes:
--
--   (!) Duplicate elements are allowed.
--   (!) Sort `Left` before `Right`.


sorts :: Ord a => [a] -> [a] -> [Either a a]
sorts (l : ls) (r : rs)

 |  l <= r     = Left l :           sorts      ls  (r : rs)
 
 |  otherwise  =          Right r : sorts (l : ls)      rs
 -- l >  r

sorts      []  (r : rs) =  Right r : sorts' Right rs
sorts (l : ls)      []  =  Left  l : sorts' Left  ls
sorts      []       []  =  []

sorts' e (i : is) =  e i : sorts' e is
sorts' _      []  =  []  


-- The difficulty for me was: "Either".
-- I forgot about it while defining and
-- did wrong in defining the empty list cases in my first definition.
--
--    sorts ls rs =  ls ++ rs
--
-- That isn't possible without constructor application,
-- because of the output type "[Either a a]". Is it?
--
-- Wish there would be GHCi in the exam.


-- Where else did I fail and didn't notice it?
-- What higher order functions would you use to define "sorts"?

-- (IThe exercises from old exams that I have and solved are at:
-- https://github.com/pascal-knodel/Programming-Paradigms-KIT-2014-2015/tree/master/3
-- )




-- Write another function that takes a list-of-lists as input
-- and outputs the result in tuples, where the first member is
-- the index of the list (counting from zero), and the second
-- is the value itself.
--
-- Example [[1,3],[2,3],[1,3,4]] should result in [(0,1),(2,1),(1,2),(0,3),(1,3),(2,3),(2,4)]


-- 2. First try:

-- I'm not sure, what do you mean by 'value itself'?
-- Why is the second tuple (2,1)? Ah, I see, is it sth. like:
--
--  [1,3] is index 0 and becomes (0,1), (03)
--  [2,3]          1             (1,2), (1,3)
--  ...
--
-- And those sorted in ascending order by the sum of the components?
-- Is it really possible to stream this computation without blocking?
-- Is the list-of-lists sorted?

