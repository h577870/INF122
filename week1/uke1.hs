-- 577870

-- 1.4
qsortrev :: Ord a => [a] -> [a]
qsortrev [] = []
qsortrev [x] = [x]
qsortrev (x : xs) = qsortrev larger ++ [x] ++ qsortrev smaller
  where
    smaller = [a | a <- xs, a <= x]
    larger = [b | b <- xs, b > x]

-- 1.5
{-
Duplikatverdier kommer ikke med i output.
-}
qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort [x] = [x]
qsort (x : xs) = qsort smaller ++ [x] ++ qsort larger
  where
    smaller = [a | a <- xs, a < x]
    larger = [b | b <- xs, b > x]

-- 2.4
last' :: [c] -> c
last' = head . reverse

-- 2.5
init' :: [a] -> [a]
init' [] = []
init' xs = reverse . tail . reverse $ xs

init'1 :: [a] -> [a]
init'1 [] = []
init'1 xs = reverse . drop 1 . reverse $ xs

--

plu :: [Int] -> Int -> [Int]
plu [] _ = []
plu xs 0 = xs
plu (x : xs) n = x + n : plu xs n

--

pali :: Eq a => [a] -> Bool
pali xs = xs == reverse xs

-- alternativ
pali' :: Eq a => [a] -> Bool
pali' = reverse >>= (==)