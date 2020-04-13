

getSum :: [Int] -> Int
getSum [] = 0
getSum (x:xs) = x + getSum xs

sumOfSublists :: [[Int]] -> [Int]
sumOfSublists x = [ getSum n | n <- x]

qSort :: [Int] -> [Int]
qSort [] = []
qSort (x:xs) = qSort [a | a <- xs, a<=x] ++ [x] ++ qSort [n | n <- xs, n>x]

sortMatrix :: [[Int]] -> [[Int]]
sortMatrix [] = []
sortMatrix (x:xs) = sortMatrix [less | less <- xs, sum less <= sum x] ++ [x] ++ sortMatrix [greater | greater <- xs, sum greater > sum x]

-- getAllSublistsNew :: [Int] -> [(Int, Int, [Int])]
-- getAllSublistsNew [] = []
-- getAllSublistsNew (x:xs) = [(length (x:xs) - length xs,  length xs, getSublists (x:xs))] ++ getAllSublistsNew xs 



getSublistsNew :: [Int] -> Int -> [(Int, Int, [Int])]
getSublistsNew [] _ = []
getSublistsNew x y= [(y,(y + length x) - 1, x)] ++ getSublistsNew (init x) y  

getAllSublistsNew :: [Int] -> Int ->[(Int, Int, [Int])]
getAllSublistsNew [] _= []
getAllSublistsNew (x:xs) y = getSublistsNew (x:xs) y ++ getAllSublistsNew (tail (x:xs)) (y+1)

-- generateSublists :: [Int] -> Int -> [(Int, Int [Int])]
-- generateSublists [] _= []
-- generateSublists [] = getAllSublistsNew 