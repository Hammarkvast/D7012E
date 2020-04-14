

getSum :: [Int] -> Int
getSum [] = 0
getSum (x:xs) = x + getSum xs

returnSumFromTuple :: (Int, Int, Int, [Int]) -> Int
returnSumFromTuple (theSum, i, j, lst) = theSum

calcSum :: [(Int, Int, Int, [Int])] -> Int
calcSum [(_, _, _, [])] = 0
calcSum ((theSum,i,j,((start:end))):xs)  = start + calcSum ((theSum,i,j,((end))):xs) + calcSum xs 


quickSort :: [(Int, Int, Int, [Int])] -> [(Int, Int, Int, [Int])]
quickSort [] = []
quickSort ((sumofLst, i, j, lst):xs) = quickSort [small | small <- xs, returnSumFromTuple small <= sumofLst] ++ 
        [(sumofLst, i, j, lst)] ++ 
        quickSort [larger | larger <- xs, returnSumFromTuple larger > sumofLst]


getSublists :: [Int] -> Int -> [(Int, Int, Int, [Int])]
getSublists [] _ = []
getSublists x start = [(getSum x, start,(start + length x) - 1, x)] ++ getSublists (init x) start  

getAllSublists :: [Int] -> Int ->[(Int, Int, Int, [Int])]
getAllSublists [] _= []
getAllSublists (x:xs) start = getSublists (x:xs) start ++ getAllSublists (tail (x:xs)) (start+1)

generateString :: [(Int, Int, Int, [Int])] -> String
generateString ((lstSum, i, j, lst):xs) 
    |  xs == [] = ""
    | otherwise = show lstSum ++ "\t" ++ show i ++ "\t" ++ show j ++ "\t" ++ show lst ++ "\n" ++ generateString xs

generateKLst :: [Int] -> Int -> [(Int, Int, Int, [Int])]
generateKLst [] _ = []
generateKlst completeList k = let allSubLsts = getAllSublists completeList 0
                                  sorted = quickSort allSubLsts
                                in take k sorted

smallestKset:: [Int] -> Int -> IO()
--smallestKset [] _ =  
smallestKset lst k = if length lst == 15 then putStrLn "End of List!" else 
    let subLists = generateKlst lst k     
        str = generateString subLists
        in putStrLn (str)




-- generateSublists :: [Int] -> Int -> [(Int, Int [Int])]
-- generateSublists [] _= []
-- generateSublists [] = getAllSublists 