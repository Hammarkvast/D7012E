--Tom Hammarkvist
module Statement(T, parse, toString, fromString, exec) where
import Prelude hiding (return, fail)
import Parser hiding (T)
import qualified Dictionary
import qualified Expr
type T = Statement
data Statement =
    Assignment String Expr.T |
    If Expr.T Statement Statement |
    Begin [Statement] |
    While Expr.T Statement |
    Write Expr.T |
    Read String |
    Repeat Statement Expr.T |
    Skip 
    deriving Show

assignment = word #- accept ":=" # Expr.parse #- require ";" >-> buildAss
buildAss (v, e) = Assignment v e


write = accept "write" -# Expr.parse #- require ";" >-> buildWrite
buildWrite e = Write e


read = accept "read" -# word #- require ";" >-> buildRead
buildRead s = Read s

skip = accept "skip" #- require ";" >-> buildSkip 
buildSkip v = Skip 

while = accept "while" -# Expr.parse #- require "do" # parse >-> buildWhile
buildWhile (e, v) = While e v

begin = accept "begin" -# iter parse #- require "end" >-> buildBegin
buildBegin s  = Begin s

if_statement = accept "if" -# Expr.parse #- require "then" # parse #- require "else" # parse >-> buildIf
buildIf ((e, s1), s2) = If e s1 s2

repeat = accept "repeat" -# parse #- require "until" # Expr.parse #- require ";" >-> buildRepeat
buildRepeat (e, v) = Repeat e v

shw :: Int -> Statement ->  String
shw prec (Assignment s expr) = s ++ " := " ++ Expr.toString expr ++ "\n"
shw prec (If expr s1 s2) = "if " ++ Expr.toString expr ++ " then " ++ "\n" ++ shw prec s1 ++ "else" ++ "\n" ++ shw prec s2 
shw prec (Begin stmnts) = "Begin\n" ++   beginHelper prec stmnts ++ "end\n" 
shw prec (While expr s) = "while " ++ Expr.toString expr ++ " do \n" ++  shw prec s
shw prec (Write expr) = "write " ++ Expr.toString expr ++ ";\n"
shw prec (Read str) = "Read " ++ str ++  ";\n"  
shw prec (Repeat s expr) = "repeat\n" ++ shw prec s ++ "until " ++ Expr.toString expr ++ ";\n"
shw prec (Skip ) = "Skip;" ++ "\n"

beginHelper:: Int -> [Statement] -> String
beginHelper _ [] = []
beginHelper prec (x:xs) = shw prec x ++  beginHelper prec xs

exec :: [T] -> Dictionary.T String Integer -> [Integer] -> [Integer]

exec (If cond thenStmts elseStmts: stmts) dict input = 
    if (Expr.value cond dict)>0 
    then exec (thenStmts: stmts) dict input
    else exec (elseStmts: stmts) dict input

exec (Skip:stmts) dict input = exec stmts dict input

exec (Begin bgnstmnt:stmnt) dict input = exec (bgnstmnt ++ stmnt) dict input

exec (Read str: stmnts) dict inputs = exec (stmnts) (Dictionary.insert (str, head (inputs)) dict) (tail inputs)

exec (Write func :stmnts) dict inputs = [x] ++ (exec (stmnts) (dict) (tail(inputs)))
    where
        x = Expr.value func dict

exec (While cond whilestmnt:stmnt) dict input = 
    if (Expr.value cond dict) > 0
    then exec (whilestmnt:While cond whilestmnt:stmnt) dict input
    else exec stmnt dict input

exec (Assignment str val:stmnt) dict input = exec (stmnt) (Dictionary.insert (str, x) dict) input 
    where
        x = Expr.value val dict

-- exec (Repeat repeatstmnt cond:stmnt) dict input =
--     if (Expr.value cond dict)<= 0
--     then exec (repeatstmnt:Repeat repeatstmnt cond:stmnt) dict input
--     else exec stmnt dict input

exec (Repeat repeatstmnt cond:stmnt) dict input =
    exec (repeatstmnt:(
         if (Expr.value cond dict) <=0
         then Repeat repeatstmnt cond
         else Skip):stmnt ) dict input

exec _ dict input = []

instance Parse Statement where
    parse = assignment ! write ! while ! Statement.read ! if_statement ! begin ! skip ! Statement.repeat
    toString = shw 0
