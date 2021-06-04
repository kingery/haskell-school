module Golf where

-- Excercise 1
l = length

skips :: [a] -> [[a]]
skips xs = map (pickEvery xs) [1..(l xs)]

pickEvery :: [a] -> Int -> [a]
pickEvery [] _ = []
pickEvery xs i
    | i > l xs  = []
    | otherwise = last fst : pickEvery snd i
        where (fst, snd) = splitAt i xs

-- alternate approach...
skips' :: [a] -> [[a]]
skips' xs = map ($ xs) (map pickEvery' [1..(l xs)])

pickEvery' :: Int -> [a] -> [a]
pickEvery' _ [] = []
pickEvery' i xs
    | i > l xs  = []
    | otherwise = last fst : pickEvery' i snd
        where (fst, snd) = splitAt i xs
 
 -- Excercise 2
localMaxima :: [Integer] -> [Integer]
localMaxima (x:y:z:xs)
    | y > x && y > z  = y : localMaxima (z:xs)
    | otherwise       = localMaxima (y:z:xs)
localMaxima _ = []

-- Excercise 3
histogram :: [Integer] -> String
histogram = drawHist . countVals

countVals :: [Integer] -> [Integer]
countVals _ = []

drawHist :: [Integer] -> String
drawHist vals = draw vals ++ "\n==========\n0123456789"

draw :: [Integer] -> String
draw _ = "foo"

