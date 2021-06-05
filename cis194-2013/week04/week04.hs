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
foldTree = foldr insert Leaf

-- insert new node into Tree with value 
-- new nodes include 
insert :: a -> Tree a -> Tree a
insert x Leaf                 = Node 0 Leaf x Leaf
insert x (Node h l z r)
    | height l > height r = Node h l z (insert x r)
    | height l < height r = Node h (insert x l) z r
    | otherwise           = Node (1 + max (height l) (height (insert x r)))
                                 (insert x l) z r
    where height Leaf = -1
          height (Node h _ _ _) = h


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
sieveSundaram n = map (\x -> 2*x + 1) $ filter (\x -> notElem x (vals n)) [1..n]

vals :: Integer -> [Integer]
vals n = [i + j + 2*i*j | i <- [1..n], j <- [1..n], 1 <= i && i <= j, i + j + 2*i*j <= n]
