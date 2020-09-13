-- A

fjern1 :: String -> Char -> String
fjern1 xs y = [x | x <- xs, x /= y]

fjern2 :: String -> Char -> String
fjern2 [] _ = []
fjern2 (x : xs) y
  | x == y = fjern2 xs y
  | otherwise = x : fjern2 xs y

-- B

tegnpos1 :: Char -> String -> [Int]
tegnpos1 a ys = [y | (x, y) <- zip ys [0 ..], x == a]

tegnpos2 :: Char -> String -> [Int]
tegnpos2 _ [] = []
tegnpos2 p xs = tegnposrec p $ zip xs [0 ..]
  where
    tegnposrec _ [] = []
    tegnposrec a (x : xs)
      | a == fst x = snd x : tegnposrec a xs
      | otherwise = tegnposrec a xs

-- C

intToList2 :: Int -> [Int]
intToList2 0 = []
intToList2 x = intToList2 (x `div` 10) ++ [x `mod` 10]

-- D.a

settSammen :: [String] -> String
settSammen [] = ""
settSammen (x : xs) = x ++ go xs
  where
    go [] = ""
    go (c : cs) = ' ' : (c ++ go cs)

-- D.b

delStrengen1 :: String -> [String]
delStrengen1 "" = []
delStrengen1 (x : xs)
  | [x] == " " = delStrengen1 xs
  | otherwise = fst filterSpace : delStrengen1 (snd filterSpace)
  where
    filterSpace = break (\p -> [p] == " ") (x : xs)

-- D.c

gdelStrengen :: String -> String -> [String]
gdelStrengen [] _ = []
gdelStrengen (x : xs) del
  | x `elem` del = gdelStrengen xs del
  | otherwise = fst filterStr : gdelStrengen (snd filterStr) del
  where
    filterStr = break (`elem` del) (x : xs)
