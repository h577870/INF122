-- 1

-- a
al1 :: [Bool] -> Bool
al1 [] = True
al1 (x : xs) = x && al1 xs

-- b
al2 :: [Bool] -> Bool
al2 = all (== True)

-- c
al3 :: [Bool] -> Bool
al3 [] = True
al3 xs = foldl (&&) True xs

-- d
al4 :: [Bool] -> Bool
al4 [] = True
al4 xs = foldr (&&) True xs

-- 2

ala :: (Bool -> Bool -> Bool) -> Bool -> [Bool] -> Bool
ala = foldr

-- 3

trekant :: Int -> IO ()
trekant n = putStr (print' 1 n)

print' :: Int -> Int -> String
print' m n
  | m == n = ch "* " m ++ "\n"
  | otherwise = ch "* " m ++ "\n" ++ print' (m + 1) n

ch :: (Eq t, Num t) => [a] -> t -> [a]
ch s m
  | m == 0 = []
  | otherwise = s ++ ch s (m - 1)

-- 4

juletre :: Int -> IO ()
juletre n = putStr (printJ 1 n)

printJ :: Int -> Int -> String
printJ m n
  | m == n = ch " " (n - m) ++ ch " *" m ++ "\n"
  | otherwise = ch " " (n - m) ++ ch " *" m ++ "\n" ++ printJ (m + 1) n
