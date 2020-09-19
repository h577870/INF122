import Data.Char (isDigit)

-- 1

-- Deriving Show in order to display in readable format.
data Ast = V Int | S String | P Ast Ast | M Ast Ast deriving (Show)

eval :: Ast -> Int
eval (V n) = n
eval (M n m) = eval n * eval m
eval (P n m) = eval n + eval m
-- Ekstra for 3.3
eval (S n) = error "Inneholder streng."

-- 2

inn :: Ast -> String
inn (V n) = show n
inn (M n m) = "(" ++ inn n ++ " * " ++ inn m ++ ")"
inn (P n m) = "(" ++ inn n ++ " + " ++ inn m ++ ")"

-- 3.1

tokenizer :: String -> [String]
tokenizer [] = []
tokenizer (x : xs)
  | x `elem` s = tokenizer xs
  | x `elem` t = [x] : tokenizer xs
  | otherwise = takeWhile (notin $ t ++ s) (x : xs) : tokenizer (dropWhile (notin $ t ++ s) (x : xs))
  where
    s = [' ']
    t = "*+()"
    notin xs = (`notElem` xs)

-- 3.2

parser :: String -> Ast
parser = fst . parseE . tokenizer

parseE :: [String] -> (Ast, [String])
parseE [] = error "Uttrykket er ikke korrekt."
parseE ("+" : xs) =
  let (e1, r1) = parseE xs
      (e2, r2) = parseE r1
   in (P e1 e2, r2)
parseE ("*" : xs) =
  let (e1, r1) = parseE xs
      (e2, r2) = parseE r1
   in (M e1 e2, r2)
parseE (x : xs) =
  if onlyDigits x
    then (V (read x :: Int), xs)
    else (S x, xs)
  where
    onlyDigits xs = takeWhile isDigit xs == xs

-- 3.3

ev :: String -> Int
ev = eval . parser

-- 3.4

infiks :: String -> String
infiks = inn . parser