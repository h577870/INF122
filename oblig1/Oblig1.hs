-- Kristoffer Davidsen, group 4
module Oblig1 where

import Data.Char (isAlpha, isDigit)

{-
Expr -> Term + Expr | Term - Expr | Term
Term -> Number * Term | Word
Word -> Letter Word | Letter
Number -> Digit Int | Digit
Letter -> a | b | c | ...| x | y | z | A | B | ...| Y |
Digit -> 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9
-}

data Ast
  = Word String
  | Num Int
  | Mult Ast Ast
  | Plus Ast Ast
  | Minus Ast Ast
  deriving (Eq, Show)

parse :: String -> Ast
parse s =
  case parseE (tokenizer s) of
    (e, []) -> e
    (_, t : _) -> error ("unexpected token" ++ show t)

parseF :: [String] -> (Ast, [String])
parseF ("(" : s) =
  let (a1, ")" : r1) = parseE s in (a1, r1)
parseF (x : s) =
  if onlyDigits x
    then (Num (read x :: Int), s)
    else (Word x, s)
  where
    onlyDigits x = takeWhile isDigit x == x
parseF [] = error "Missing expression."

parseT :: [String] -> (Ast, [String])
parseT xs =
  let (a1, r1) = parseF xs
   in if null r1
        then (a1, r1)
        else
          if head r1 == "*"
            then let (a2, r2) = parseT (tail r1) in (Mult a1 a2, r2)
            else (a1, r1)

parseE :: [String] -> (Ast, [String])
parseE xs =
  let (a1, r1) = parseT xs
   in if null r1
        then (a1, r1)
        else
          if head r1 == "+"
            then let (a2, r2) = parseE (tail r1) in (Plus a1 a2, r2)
            else
              if head r1 == "-"
                then let (a2, r2) = parseE (tail r1) in (Minus a1 a2, r2)
                else (a1, r1)

tokenizer :: String -> [String]
tokenizer [] = []
tokenizer (x : xs)
  | x `elem` s = tokenizer xs
  | x `elem` t = [x] : tokenizer xs
  | otherwise =
    takeWhile (notin $ t ++ s) (x : xs) :
    tokenizer (dropWhile (notin $ t ++ s) (x : xs))
  where
    s = [' ']
    t = "*+-()"
    notin xs = (`notElem` xs)

viss :: Ast -> String
viss ast = viss' ast 0

whitespace :: Int -> String
whitespace n = [' ' | n <- [0 .. n -1]]

viss' :: Ast -> Int -> String
viss' (Num x) n = whitespace n ++ show (Num x) ++ "\n"
viss' (Word w) n = whitespace n ++ show (Word w) ++ "\n"
viss' (Plus a1 a2) n = whitespace n ++ "Plus" ++ "\n" ++ viss' a1 (n + 3) ++ viss' a2 (n + 3)
viss' (Mult a1 a2) n = whitespace n ++ "Mult" ++ "\n" ++ viss' a1 (n + 3) ++ viss' a2 (n + 3)
viss' (Minus a1 a2) n = whitespace n ++ "Minus" ++ "\n" ++ viss' a1 (n + 3) ++ viss' a2 (n + 3)

vis :: Ast -> IO ()
vis ast = putStr (viss ast)

eval :: Ast -> String
eval str = eval' str

eval' :: Ast -> String
eval' (Num n) = show n
eval' (Word w) = w
eval' (Plus a1 a2)
  | all isDigit (eval' a1) || all isDigit (eval' a2) = error "Illegal to add type Num with type Word."
  | otherwise = eval' a1 ++ eval' a2
eval' (Minus a1 a2)
  | all isDigit (eval' a1) || all isDigit (eval' a2) = error "Illegal to subtract type Num with type Word."
  | otherwise = diff' (eval' a1) (eval' a2)
eval' (Mult a1 a2)
  | all isAlpha (eval' a1) && all isDigit (eval' a2) = error "Illegal argument positions. Hint: Make argument #1 hold type Num."
  | all isAlpha (eval' a1) = error "Ast #1 is not a number"
  | all isDigit (eval' a1) && all isDigit (eval' a2) = error "Illegal to multiply two arguments of type Num."
  | otherwise = concat $ replicate (read $ eval' a1) (eval' a2)

diff' :: Eq a => [a] -> [a] -> [a]
diff' x y = foldl (flip delete) x y

delete :: Eq a => a -> [a] -> [a]
delete = deleteBy (==)

deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
deleteBy _ x [] = []
deleteBy eq x (y : ys) = if x `eq` y then ys else y : deleteBy eq x ys