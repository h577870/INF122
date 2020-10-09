-- 7.1
{-
[f x | x <- xs, p x]
-}

mapandfilter :: (a -> b) -> (a -> Bool) -> [a] -> [b]
mapandfilter f p xs = map f . filter p $ xs

-- 7.4

dec2int :: [Int] -> Int
dec2int xs = foldl (\x -> \y -> 10 * x + y) 0 xs

-- 7.5
{-
Without looking at the definitions from the standard prelude,
define the higher-order library function curry that converts a function on pairs into a curried function,
and, conversely, the function uncurry that converts a curried function
with two arguments into a function on pairs.
Hint: first write down the types of the two functions.
-}

curry' :: ((a, b) -> c) -> a -> b -> c
curry' f x y = f (x, y)

uncurry' :: (a -> b -> c) -> ((a, b) -> c)
uncurry' f (x, y) = f x y

-- 7.9

altMap :: (a -> b) -> (a -> b) -> [a] -> [b]
altMap _ _ [] = []
altMap x _ [r] = [x r]
altMap x y (r : s : ts) = x r : y s : altMap x y ts

-- 8.5

data Expr = Val Int | Add Expr Expr deriving (Show)

folde :: (Int -> a) -> (a -> a -> a) -> Expr -> a
folde f g expr =
  case expr of
    Val r -> f r
    Add s t -> g (folde f g s) (folde f g t)

-- 8.6

eval :: Expr -> Int
eval expr = folde (\x -> x) (+) expr

size :: Expr -> Int
size expr = folde (const 1) (+) expr

infiks :: Expr -> String
infiks (Val n) = show n
infiks (Add r s) = "(" ++ infiks r ++ "+" ++ infiks s ++ ")"

prefiks :: Expr -> String
prefiks (Add r s) = "+" ++ prefiks r ++ prefiks s
prefiks (Val n) = show n

postfiks :: Expr -> String
postfiks (Add r s) = postfiks r ++ postfiks s ++ "+"
postfiks (Val n) = show n