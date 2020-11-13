-- Kristoffer Davidsen

import Data.List ((\\))

type Board = [Pos]

type Pos = (Int, Int)

clr :: IO ()
clr = putStr "\ESC[2J"

isAlive :: Board -> Pos -> Bool
isAlive b p = p `elem` b

isEmpty :: Board -> Pos -> Bool
isEmpty b p = not (isAlive b p)

readMatrix :: [String] -> Board
readMatrix xs = [(x, y) | x <- [0 .. length xs -1], y <- [0 .. length xs -1], xs !! x !! y /= '.']

writeTop :: (Show a, Integral a) => a -> IO ()
writeTop nR = writeAt (lft + 1, 0) (concat [show (mod i 10) ++ " " | i <- [1 .. nR]] ++ "\n")

lft :: Integer
lft = 3

writeAt :: (Show a1, Show a2) => (a2, a1) -> String -> IO ()
writeAt (x, y) xs = do
  goto (x, y)
  putStr xs

goto :: (Show a1, Show a2) => (a2, a1) -> IO ()
goto (x, y) = putStr ("\ESC[" ++ show y ++ ";" ++ show x ++ "H")

writeRow :: (Show a1, Ord a1, Num a1, Num a2, Enum a2) => a1 -> a2 -> IO ()
writeRow i nR = do
  writeAt (if i > 9 then lft - 2 else lft - 1, 1 + i) (show i)
  mapM_ (\i -> putStr " .") [1 .. nR]
  putStrLn ""

brett :: (Show a1, Integral a1) => a1 -> IO ()
brett nR
  | nR < 99 && nR > 0 = do
    clr
    writeTop nR
    mapM_ (`writeRow` nR) [1 .. nR]
  | otherwise = error "Number must be bigger than 0 and lower than 99"

main :: IO ()
main = do
  clr
  putStrLn "Skriv inn en initialiseringskommando:"
  initialize <- getLine
  let word = words initialize
  let headoftail = read (head $ tail word)
  let board = []
  case head word of
    "c" -> do
      brett headoftail
      repeat' board headoftail [] []
    "r" -> do
      readFile' (unwords (tail word))
      repeat' board headoftail [] []
    _ -> main

repeat' :: Board -> Int -> [Int] -> [Int] -> IO ()
repeat' board length s b = do
  cmd <- getLine
  let commands = words cmd
  if not (null commands)
    then command commands board length s b
    else repeat' board length s b

convert :: [String] -> [Pos]
convert [] = []
convert (x : y : xs)
  | null x || null y = []
  | otherwise = (read x, read y) : convert xs

command :: [String] -> Board -> Int -> [Int] -> [Int] -> IO ()
command xs board nR s b
  | head xs == "quit" = return ()
  | head xs == "c" = do
    brett $ read (last xs)
    repeat' board nR s b
  | head xs == "n" = do
    let newn = convert (tail xs)
    writeCells newn nR "O"
    repeat' (rmdups board ++ newn) nR s b
  | head xs == "e" = do
    let newe = convert (tail xs)
    writeCells newe nR "."
    repeat' (board \\ newe) nR s b
  | head xs == "b" = do
    let newb = [read (xs !! 1) .. read (xs !! 2)]
    clearMessage nR
    repeat' board nR s newb
  | head xs == "s" = do
    let news = [read (xs !! 1) .. read (xs !! 2)]
    clearMessage nR
    repeat' board nR news b
  | head xs == "w" = do
    writeToScreen nR board s b
    repeat' board nR s b
  | head xs == "r" = do
    readFile' (tail $ head xs)
    repeat' board nR s b
  | head xs == "enter" = do
    let newboard = nextgen board nR s b
    if newboard == board
      then return ()
      else do
        writeCells (newboard \\ board) nR "0"
        writeCells (board \\ newboard) nR "."
        repeat' newboard nR s b
  | otherwise = repeat' board nR s b

readFile' :: String -> IO ()
readFile' name = do
  string <- readFile name
  case tokenizer string of
    [] -> return ()
    (n : "s" : x : y : "b" : z : q : xs) -> do
      brett (read n)
      writeCells (convert xs) (read n) "0"
      repeat' (convert xs) (read n) [read x .. read y] [read z .. read q]
    (n : "b" : x : y : "s" : z : q : xs) -> do
      brett (read n)
      writeCells (convert xs) (read n) "0"
      repeat' (convert xs) (read n) [read x .. read y] [read z .. read q]
    _ -> return ()

tokenizer :: String -> [String]
tokenizer [] = []
tokenizer (x : xs)
  | x `elem` t = tokenizer xs
  | otherwise = [x] : tokenizer xs
  where
    t = "*+(), "

writeToScreen :: Int -> Board -> [Int] -> [Int] -> IO ()
writeToScreen nR board s b = do
  let string = str ++ unwords (foldl (\x (a, b) -> "(" : show a : ", " : show b : ")" : x) [] board)
  clearMessage nR
  writeAt (0, nR + 4) string
  goto (0, nR + 2)
  where
    str =
      "b (" ++ show (head b) ++ ", " ++ show (last b) ++ "), "
        ++ "s ("
        ++ show (head s)
        ++ ", "
        ++ show (last s)
        ++ "), tupler: "

writeCells :: [Pos] -> Int -> String -> IO ()
writeCells [] _ _ = return ()
writeCells (b : xs) nR str
  | null b = return ()
  | otherwise = do
    writeAt (getPos b) str
    clearMessage nR
    writeCells xs nR str

getPos :: Pos -> Pos
getPos (x, y) = (x + (x - 1) + 3, y + 1)

liveneighbs :: Board -> Pos -> Int
liveneighbs b p = length (filter (isAlive b) (neighbours (length b) p))

neighbours :: Int -> Pos -> [Pos]
neighbours nR (x, y) =
  map
    (wrap nR)
    [ (x - 1, y - 1),
      (x, y -1),
      (x + 1, y -1),
      (x -1, y),
      (x + 1, y),
      (x -1, y + 1),
      (x, y + 1),
      (x + 1, y + 1)
    ]

wrap :: Int -> Pos -> Pos
wrap nR (x, y) = (((x - 1) `mod` nR) + 1, ((y - 1) `mod` nR) + 1)

survivors :: [Int] -> Board -> [Pos]
survivors xs board = [p | p <- board, liveneighbs board p `elem` xs]

births :: Int -> [Int] -> Board -> [Pos]
births nR xs b =
  [p | p <- rmdups (concatMap (neighbours nR) b), isEmpty b p, fst p < nR, snd p < nR, liveneighbs b p `elem` xs]

nextgen :: Board -> Int -> [Int] -> [Int] -> Board
nextgen board nR s b = survivors s board ++ births nR b board

rmdups :: Eq a => [a] -> [a]
rmdups [] = []
rmdups (x : xs) = x : rmdups (filter (/= x) xs)

clearMessage :: (Show a1, Num a1) => a1 -> IO ()
clearMessage nR = do
  clearLine (0, nR + 4) -- Eventuelle feilmeldinger på denne plassen
  clearLine (0, nR + 3)
  clearLine (0, nR + 2)

clearLine :: (Show a1, Show a2) => (a2, a1) -> IO ()
clearLine (x, y) = do
  goto (x, y)
  putStr "\ESC[2K"
