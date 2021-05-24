-- Excercise 1
toDigits :: Integer -> [Integer]
toDigits x = reverse $ toDigitsRev x

toDigitsRev :: Integer -> [Integer]
toDigitsRev x
    | x <= 0    = []
    | otherwise = (mod x 10) : toDigitsRev (div x 10)

-- Excercise 2
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []     = []
doubleEveryOther [x]    = [x]
doubleEveryOther (x:xs)
    | mod (length xs) 2 == 0 = x : doubleEveryOther xs     --odd length list
    | otherwise              = (2*x) : doubleEveryOther xs --even length list

-- Excercise 3
sumDigits :: [Integer] -> Integer
sumDigits []     = 0
sumDigits (x:xs) = (mod x 10) + (div x 10) + (sumDigits xs)

-- Excercise 4
validate :: Integer -> Bool
validate x = mod (sumDigits $ doubleEveryOther $ toDigits x) 10 == 0

-- Excercise 5
type Peg = String
type Move = (Peg, Peg)
-- hanoi n a b c: move n pegs from a to b using c as temp storage
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = (hanoi (n-1) a c b) ++ [(a, b)] ++ (hanoi (n-1) c b a)

-- Excercise 6
-- hanoi' n a b c d: move n pegs from a to b using c and d as temp storage
hanoi' :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi' 0 _ _ _ _ = []
hanoi' n a b c d = []
