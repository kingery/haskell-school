speller :: [[Char]] -> [Char]
speller [] = ""
speller [x,y] = spell x ++ ", and " ++ spell y
speller (x:xs) = spell x ++ ", " ++ speller xs

spell :: [Char] -> [Char]
spell [] = ""
spell (x:xs) = [x] ++ " is for " ++ (x:xs)

