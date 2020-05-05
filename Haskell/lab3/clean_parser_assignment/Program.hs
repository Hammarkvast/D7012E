module Program(T, parse, fromString, toString, exec) where
import Parser hiding (T)
import qualified Statement
import qualified Dictionary
import Prelude hiding (return, fail)
newtype T = Program ([Statement.T]) 
  deriving Show -- to be defined
instance Parse T where
  parse = iter Statement.parse >-> Program
  toString (Program program) = foldl (++) "" (map Statement.toString program) 
             
exec (Program lst) input = Statement.exec lst Dictionary.empty input 
