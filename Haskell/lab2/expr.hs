-- Tom Hammarkvist
-- Code to Haskell lab assignment 2 in the course D7012E by Håkan Jonsson
import Data.Char

data EXPR = Const Int
     | Var String
     | Op String EXPR EXPR
     | App String EXPR deriving (Eq, Ord, Show)

parse :: String -> EXPR
parse = fst . buildexpr
  where
    notfirst p (_,[]) = True
    notfirst p (_,x:xs) = not (p x)
    
    buildnumber :: String -> (EXPR,String)
    buildnumber xs = until (notfirst isDigit) accdigits (Const 0, xs)
      where
        accdigits :: (EXPR,String) -> (EXPR,String)
        accdigits (Const n, y:ys) = (Const(10*n+(ord y - 48)), ys)
    
    buildvar :: String -> (EXPR,String)
    buildvar xs = until (notfirst isLetter) accletters (Var "", xs)
      where
        accletters :: (EXPR,String) -> (EXPR,String)
        accletters (Var s, y:ys) = (Var (s ++[y]), ys)
    
    
    buildexpr :: String -> (EXPR,String)
    buildexpr xs = until (notfirst (\c -> c=='-' || c=='+')) accterms (buildterm xs)
      where
        accterms :: (EXPR,String) -> (EXPR,String)
        accterms (term, y:ys) = (Op (y:[]) term term1, zs)
          where
            (term1,zs) = buildterm ys
    
    buildterm :: String -> (EXPR,String)
    buildterm xs = until (notfirst (\c -> c=='*' || c=='/')) accfactors (buildfactor xs)
      where
        accfactors :: (EXPR,String) -> (EXPR,String)  
        accfactors (fact, y:ys) = (Op (y:[]) fact fact1, zs)
          where
            (fact1,zs) = buildfactor ys
    
    buildfactor :: String -> (EXPR,String)
    buildfactor [] = error "missing factor"
    buildfactor ('(':xs) =  case buildexpr xs of (e, ')':ws) -> (e, ws); _ -> error "missing factor"
    buildfactor (x:xs)
      | isDigit x = buildnumber (x:xs)
      | isLetter x = case buildvar (x:xs) of
                       (Var s, '(':zs) -> let (e,ws)=buildfactor ('(':zs) in (App s e,ws)
                       p -> p
      | otherwise = error "illegal symbol"

unparse :: EXPR -> String
unparse (Const n) = show n
unparse (Var s) = s
unparse (Op oper e1 e2) = "(" ++ unparse e1 ++ oper ++ unparse e2 ++ ")"
unparse (App str arg) = str ++  "(" ++ unparse arg ++ ")"

eval :: EXPR -> [(String,Float)] -> Float
eval (Const n) _ = fromIntegral n
eval (Var x) env = case lookup x env of Just y -> y ; _ -> error (x ++ " undefined")
eval (Op "+" left right) env = eval left env + eval right env
eval (Op "-" left right) env = eval left env - eval right env
eval (Op "*" left right) env = eval left env * eval right env
eval (Op "/" left right) env = eval left env / eval right env
eval (App "sin" right) env = sin (eval right env)
eval (App "cos" right) env = cos (eval right env)
eval (App "log" right) env = log (eval right env)
eval (App "exp" right) env = exp (eval right env)

diff :: EXPR -> EXPR -> EXPR
diff _ (Const _) = Const 0
diff (Var id) (Var id2)
  | id == id2 = Const 1
  | otherwise = Const 0
diff v (Op "+" e1 e2) = Op "+" (diff v e1) (diff v e2)
diff v (Op "-" e1 e2) = Op "-" (diff v e1) (diff v e2)
diff v (Op "*" e1 e2) =
  Op "+" (Op "*" (diff v e1) e2) (Op "*" e1 (diff v e2))
diff v (Op "/" e1 e2) =
  Op "/" (Op "-" (Op "*" (diff v e1) e1) (Op "*" e1 (diff v e2))) (Op "*" e2 e2)
diff v (App "sin" e1) = Op "*" (diff v e1) (App "cos" e1)
diff v (App "cos" e1) = Op "*" (Op "-" (Const 0) (Const 1)) (Op "*" (diff v e1) (App "sin" e1))
diff v (App "exp" e1) = Op "*" (diff v e1) (App "exp" e1)
diff v (App "log" e1) = Op "*" (diff v e1) (Op "/" (Const 1) e1)
diff _ _ = error "can not compute the derivative"

mkfun :: (EXPR, EXPR) -> (Float -> Float)
mkfun (body, var) =  \x -> eval body [(unparse var, x)]

findzero :: String -> String -> Float -> Float
findzero s1 s2 x0 = newtonRahpson x y x0 where
  x = mkfun ((diff (parse s1) (parse s2)), parse s1) 
  y = mkfun (parse s2,parse s1) 

newtonRahpson :: (Float -> Float) -> (Float -> Float) -> Float -> Float
newtonRahpson fPrime f x0 
  | abs (x0 - x) >= 0.0001 = newtonRahpson fPrime f x
  | otherwise = x
  where
    x = x0 - (f x0/ fPrime x0)




simplify :: EXPR -> EXPR
simplify (Const n) = Const n
simplify (Var id) = Var id
simplify (Op oper left right) =
  let (lefts,rights) = (simplify left, simplify right) in
    case (oper, lefts, rights) of
      ("+",e,Const 0) -> e
      ("+",Const 0,e) -> e
      ("*",e,Const 0) -> Const 0
      ("*",Const 0,e) -> Const 0
      ("*",e,Const 1) -> e
      ("*",Const 1,e) -> e
      ("-",e,Const 0) -> e
      ("/",e,Const 1) -> e
      ("/",le, re)     -> if left == right then Const 1 else Op "/" le re
      ("-",le,re)     -> if left==right then Const 0 else Op "-" le re

      (op,le,re)      -> Op op le re
simplify (App func arg) = 
  let (argument) = (simplify arg) in 
    case (func, arg) of 
      ("sin", e) -> App "sin" (simplify(e))
      ("cos", e) -> App "cos" (simplify(e))
      ("exp", e) -> App "exp" (simplify(e))
      ("log", e) -> App "log" (simplify(e))