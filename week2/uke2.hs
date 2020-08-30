-- 3.1
{-
['a','b','c'] :: [Char]
('a','b','c') :: (Char, Char, Char)
[(False, '0'), (True, '1')] :: [(Bool, Char)]
([False, True], ['0','1']) :: ([Bool], [Char])
[tail, init, reverse] :: [[a] -> [a]]
-}

-- B -> 3.3
second :: [a] -> a
second = head . tail

swap :: (b, a) -> (a, b)
swap (x, y) = (y, x)

pair :: a -> b -> (a, b)
pair x y = (x, y)

double :: Num a => a -> a
double x = x * 2

palindrome :: Eq a => [a] -> Bool
palindrome xs = xs == reverse xs

twice :: (t -> t) -> t -> t
twice f x = f (f x)

-- C
{-
False :: Bool
5 + 8 :: Num a => a
(+) 2 :: Num a => a -> a
(+2) :: Num a => a -> a
(["foo", "bar"], 'a') :: ([[Char]], Char)
[(True, []), (False, [['a']])] :: [(Bool, [[Char]])]
\x y -> y !! x :: Int -> [a] -> a
[take, drop, \x y -> (y !! x)] :: Uttrykket har ikke en type.
[take, drop, \x y -> [y !! x]] :: [Int -> [a] -> [a]]
-}

foo1 :: a -> b -> (a, b)
foo1 x y = (x, y)

foo2 :: a -> b -> (a, b)
foo2 x = \y -> (x, y)

foo3 :: a -> b -> (a, b)
foo3 = \x y -> (x, y)

foo4 :: a -> b -> (a, b)
foo4 = \x -> \y -> (x, y)

foo5 :: b -> a -> (a, b)
foo5 = \x -> \y -> (y, x)

foo6 :: a -> b -> (a, b)
foo6 = \y -> \x -> (y, x)

{-
foo1, foo2, foo3, foo4, foo6 er ekvivalente.
-}

-- E
f1 :: a -> (a, a)
f1 x = (x, x)

f2 :: (a, b) -> a
f2 (x, y) = x

f3 :: (a, b) -> b
f3 (x, y) = y

f4 :: a -> b -> a
f4 x y = x

f5 :: a -> b -> b
f5 x y = y

-- F
f :: Num t => t -> t -> t
f x y = x + y

g :: Num t => (t, t) -> t
g (x, y) = x + y
