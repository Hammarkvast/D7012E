threeDifferent :: Int -> Int -> Int -> Bool
threeDifferent a b c 
    |a /= b && a/=c && b/=c = True
    |otherwise = False 


threeDifferent2 :: Int -> Int -> Int -> Bool 
threeDifferent2 a b c = if a /= b && a /=c && b/=c then True else False


twoDifferent :: Int -> Int -> Bool
twoDifferent x y 
    | x/=y = True
    | otherwise = False

fourDifferent :: Int -> Int -> Int -> Int -> Bool
fourDifferent a b c d 
    |abc && bcd && ad = True
    |otherwise = False
    where
        abc = threeDifferent a b c 
        bcd = threeDifferent b c d 
        ad = twoDifferent a d


sumlist :: [Int]-> Int
sumlist [] = 0
sumlist (h:t) = h + sumlist t


comprehension:: [Int] -> [[Int]]
comprehension x = y where
    y = [[y | y <- x , y `mod` 2 == 0]]




getSublist :: [Int] -> Int -> [[Int]]
getSublist x y = [take y x]

getSublists :: [Int] -> Int -> [[Int]]
getSublists x 0 = [[]]
getSublists x y = getSublist x y ++ getSublists x (y-1)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

