getSublist :: [Int] -> Int -> [[Int]]
getSublist x y = [take y x]

getSublists :: [Int] -> Int -> [[Int]]
getSublists x 0 = []
getSublists x y = getSublist x y ++ getSublists x (y-1)

getAllSublists :: [Int] -> [[Int]]
getAllSublists [] = []
getAllSublists (x:xs)  = getSublists (x:xs) (length (x:xs)) ++ getAllSublists xs  

