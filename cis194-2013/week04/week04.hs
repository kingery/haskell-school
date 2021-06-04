-- Exercise 1
fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
    | even x    = (x - 2) * fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . (filter even)

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n     = n + fun2 (n `div` 2)
       | otherwise  = fun2 (3 * n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (>1) . iterate myfun
    where myfun x = if (even x) then div x 2 else 3*x + 1

-- Exercise 2
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
    deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr addNode Leaf

addNode :: a -> Tree a -> Tree a
addNode _ _ = Leaf


-- Exersice 3
xor :: [Bool] -> Bool
xor = foldl func False
    where func x y = if x == True then x && not y else x || y

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x -> (:) (f x)) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = f base (head xs)


-- Exercise 4
sieveSundaram :: Integer -> [Integer]
sieveSundaram _ = []

cartProd :: [a] -> [b] -> [(a,b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]
