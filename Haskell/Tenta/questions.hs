--2017-05-26

--Q1

prefix :: Int -> [a] -> [a]
prefix _ [] = []
prefix n (x:xs) = if n <= 0 then
    take 0 (x:xs)
else
    if n > length (x:xs) then
        take (length (x:xs)) (x:xs)
    else
        take n (x:xs)
    
suffix :: Int -> [a] -> [a]
suffix _ [] = []
suffix n (x:xs) = if n <= 0 then
    take 0 (x:xs)
else
    if n > length (x:xs) then
        take (length (x:xs)) (x:xs)
    else
        drop ((length (x:xs)) - n) (x:xs)

--Q2
data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)

flatten:: Tree a -> [a]
flatten x = inorder x where
    inorder (Leaf x) = [x]
    inorder (Node leftSub rightSub) = inorder (leftSub) ++ inorder (rightSub)

-- balance :: Tree a -> Tree a     
-- balance tree = 
--     let x = flatten (tree) in
--     if length x == 1 then
--         Leaf x
--     else
--         balance (Node lSubtree rSubtree) where
--             lSubtree = tail (prefix (length x `div` 2) x) where
--                 x = flatten (tree)
--             rSubtree = head (suffix (length x `div` 2) x) where
--                 x = flatten (tree)


--Q3
cashreg:: IO Int
cashreg 
  = do line <- getLine
       if (line == "e") then
            return 0
        else do
            if line == "" then
                return 0
            else 
                do x <- cashreg
                   return ((read line) + x)    

goUntilEmpty :: IO ()
goUntilEmpty
  = do line <- getLine
       if (line == [])
          then return () 
          else (do putStrLn line
                   goUntilEmpty)

--Q4
mapFoldr :: (a -> b) -> [a] -> [b]
mapFoldr f = foldr (\x xs -> f x:xs) []

foldrRev :: [a] -> [a]
foldrRev = foldr (\x ra -> ra ++ [x]) []


--20170816
--Q1

append :: [a] -> [a] -> [a]
append [] ys = ys
append (x:xs) ys = x:(append xs ys)

concat1::[[a]] -> [a]
concat1 [] = []
concat1 (x:xs) = append x (concat1 xs)

--Q2
data TT a = NodeTT (a) (TT a) (TT a) (TT a) | LeafTT

prune :: TT a -> TT a   
prune LeafTT = LeafTT
prune (NodeTT _ LeafTT LeafTT LeafTT ) = LeafTT 
prune (NodeTT a tree1 tree2 tree3) = NodeTT a (prune tree1) (prune tree2) (prune tree3) 

calculator:: IO ()
calculator = calculator' []
calculator' a =
  do
    putStr "Enter number: "
    b <- getLine
    if b == "e" then
      return ()
    else if b == "" then
      do
        putStr ("Numbers entered: " ++ format a ++ "\nAccumulated sum: " ++ show (calc a) ++ "\nSum reset.\n")
        calculator' []
    else
        calculator' (a ++ [read b])
  where
    format :: [Int] -> String
    format [] = "(none)"
    format [a] = show a
    format (x:xs) = show x ++ ", " ++ format xs
    calc :: [Int] -> Int
    calc [] = 0
    calc (x:xs) = x + calc xs


myTail:: [a] -> [a]
myTail [] = []
myTail (x:xs) = xs

myInit :: [a] -> [a]
myInit [_] = []
myInit (x:xs) = x:(myInit xs)

suffixes :: [a] -> [[a]]
suffixes [] = []
suffixes lst = lst:(suffixes (myTail lst))

prefixes :: [a] -> [[a]]
prefixes [] = []
prefixes lst = lst:(prefixes (myInit lst))

data BinTree a = NodeBin (BinTree a) (BinTree a) | BinLeaf a deriving (Show, Ord, Eq)


-- sumTree :: BinTree a -> Int
-- sumTree (BinLeaf a) = a
-- sumTree (NodeBin (binTree1) (binTree2)) = (sumTree (binTree1)) + (sumTree (binTree2))


-- sumTree1 :: BinTree a -> Int
-- sumTree1 (BinLeaf x) = x
-- sumTree1 (NodeBin left right) = sumTree1 left + sumTree1 right


myMap :: (a -> b) -> [a] -> [b]
myMap f = foldr (\x xs -> f x:xs) []

-- myFilter :: (a -> Bool) -> [a] -> [a]
-- myFilter

evenCubeSum1 n = foldr (\x y -> x*x*x+y) 0 [2,4..n]

periodise :: [a] -> [a]
periodise lst = lst ++ periodise (reverse lst)

data BranchingTree a = LeafBT a | NodeBT [BranchingTree a] deriving (Ord, Eq, Show)

mapBT :: (a -> b) -> BranchingTree a -> BranchingTree b
mapBT f (LeafBT x) = LeafBT (f x)
mapBT f (NodeBT lst) = NodeBT (map (mapBT f) lst)

myConcatMap :: ([a] -> [b]) -> [[a]] -> [b]
myConcatMap f = foldr (\x xs -> f x ++ xs) []

fix f x = f (fix f) x

subst f g x = f x (g x)

totalSum :: [Int] -> Int
totalSum lst = foldr (+) 0 lst

f1 x y = y x
-- f2 = map (.)
f3 f x y = f (f x) y
-- f = (t1 -> t)
-- x = t1
-- y = t2
-- f3 = (t1- > t) -> t1 -> t2 -> t ?


triangle :: Int -> IO ()
triangle 0 = putStr ""
triangle n = do putStrLn (replicate n '*')
                triangle (n-1)


mergeUnq [] b = b
mergeUnq a [] = a
mergeUnq (a:as) (b:bs)
   | a < b     = a : mergeUnq as (b:bs)
   | b < a     = b : mergeUnq (a:as) bs
   | otherwise = mergeUnq (a:as) bs
   
ham :: [Integer]
ham = 1 : mergeUnq
           (map (2*) ham)
           (mergeUnq (map (3*) ham) (map (5*) ham))


--2018-04-25

-- Q1

-- 1 a)

-- 1) valid, its a char.
-- 2 invalid, you cant do ":" operator with two lists, the left operand
-- has to look like a element in the right list. : takes an a and a list [a] this expression
-- has two lists as input, which makes it incorrect.
-- 3)


isOrdered :: Ord t => [t] -> Bool
isOrdered [] = True
isOrdered (x:xs) = if (xs == []) 
    then True
    else 
        if (x <= head (xs)) then
            isOrdered (xs)
        else
            False


mkList :: Int -> Int -> [Int]
mkList n m = if (n == m) then
    n:(mkList n m)
    else 
        n:m:(mkList (m + (m-n)) ((m + (m-n)) + (m-n)))

data AlgTree a = AlgNode (AlgTree a) (AlgTree a) (AlgTree a) (a, Int) | AlgLeaf deriving (Eq, Show)

extract :: Int -> AlgTree a -> [a]
extract n AlgLeaf = []
extract n (AlgNode (node1) (node2) (node3) (dataType, number)) = 
    if (n == number) then
        dataType:(extract n (node1) ++ extract n (node2) ++ extract n (node3))
    else
       extract n (node1) ++ extract n (node2) ++ extract n (node3)

-- 180822

--1 a) [Int] -> [Int]
--1 b) xs :: [t1]
--     x :: t1
--     f :: (t1 -> t)
-- fd:: -> [(t1 -> t) -> [t1] -> t]
-- 1 c) x:xs :: [t]
--      ys ::[t]
--      fb :: [t] -> [t] -> [t]
-- 1 d) [t] -> Bool


fb [] ys = ys
fb (x:xs) ys = x : fb xs ys

fc (x:y:z) = x < y && fc (y:z)
fc _ = True

(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

data T a = NodeT a (T a) (T a) | LeafT a deriving (Eq, Ord, Show)

transform :: T a -> (a -> a) -> T a
transform (LeafT val) f = LeafT (f val)
transform (NodeT val (tTree1) (tTree2)) f = NodeT (f val) (transform tTree1 f) (transform tTree2 f) 


valid :: [[Int]] -> (Int, Int) -> Bool
valid (firstRow:mtrx) (column, row) = if (column <= c && row <= r) 
    then
        True
    else
        False 
    where
        c = length firstRow
        r = length (firstRow:mtrx)

a :: [[Int]] -> (Int, Int) -> Int
a m (i, j) = (m !! i) !! j
