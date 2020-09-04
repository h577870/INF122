import Data.List ((\\))

-- 4.5
(&&) :: Bool -> Bool -> Bool
x && y =
  if x == True
    then if (y == True) then True else False
    else False

(||) :: Bool -> Bool -> Bool
x || y =
  if x == False
    then if y == False then False else True
    else True

-- 4.7
-- original:
mult :: Int -> Int -> Int -> Int
mult x y z = x * y * z

-- løsning:
mult' :: Int -> Int -> Int -> Int
mult' = \x -> \y -> \z -> x * y * z

-- alternativ løsning:
multalt :: Int -> Int -> Int -> Int
multalt = \x y z -> x * y * z

-- 5.6
factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

perfects :: Int -> [Int]
perfects n = [x | x <- [1 .. n], (sum (factors x) - x) == x]

-- 5.7
foo :: [(Integer, Integer)]
foo = concat [[(x, y) | x <- [1, 2]] | y <- [3, 4]]

-- 5.9
scalarproduct :: [Int] -> [Int] -> Int
scalarproduct x y = sum [a * b | (a, b) <- zip x y]

-- C

rem1 :: Eq a => [a] -> a -> [a]
rem1 [] _ = []
rem1 (x : xs) y
  | x == y = xs
  | otherwise = x : rem1 xs y

-- D

diff :: Eq a => [a] -> [a] -> [a]
diff x [] = x
diff [] y = []
diff x y = x \\ y

-- E

luhnDouble :: Int -> Int
luhnDouble x =
  if double > 9 then double - 9 else double
  where
    double = x * 2

luhn :: Int -> Int -> Int -> Int -> Bool
luhn w x y z = result `mod` 10 == 0
  where
    result = luhnDouble w + x + luhnDouble y + z