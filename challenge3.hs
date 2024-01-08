import Data.Char
import Parsing
import Control.Monad
import Data.List hiding (splitAt)
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import Control.Monad.STM (check)
import Data.Maybe (fromJust)

data LamExpr = LamApp LamExpr LamExpr | LamAbs Int LamExpr | LamVar Int deriving (Show,Eq,Read)


--use function like one of the assessed sheets to convert to required string 
--exercise A7 
--maybe convert it to alpha normal form instead of using strings 

getLamAbs :: LamExpr -> [Int]
getLamAbs (LamApp x y) = getLamAbs x ++ getLamAbs y 
getLamAbs (LamAbs n x) = n : getLamAbs x 
getLamAbs (LamVar n) = []

--the first int represents what the lamda expression is e.g //x0 = 0 
--the secodn int will be the int you want to change it to e.g lamAbs 1 (var 1) change both to 0 
changeAbs :: LamExpr -> Int -> Int -> LamExpr
-- changeAbs (LamApp x y) oldN newN = fmap (flip LamApp y) (changeAbs x oldN newN) <|> fmap (LamApp x) (changeAbs y oldN newN)
changeAbs (LamApp x y) oldN newN = LamApp (changeAbs x oldN newN) (changeAbs y oldN newN)
changeAbs ((LamAbs n x)) oldN newN | n == oldN = (LamAbs newN (changeAbs (changeVars x oldN newN) oldN newN)) 
                                   | otherwise = (LamAbs n) (changeAbs x oldN newN)
changeAbs ((LamVar n)) oldN newN  = LamVar n                                

--this function will go into the LamAbs and check if the variables n == oldN 
changeVars :: LamExpr -> Int -> Int -> LamExpr
changeVars (LamApp x y) oldN newN = LamApp (changeVars x oldN newN) (changeVars y oldN newN)
changeVars (LamAbs n x) oldN newN = LamAbs n (changeVars x oldN newN )
changeVars (LamVar n) oldN newN | n == oldN = LamVar newN 
                                | otherwise = LamVar n  


--will take a list of ints --> the old abs numbers 
--a counter 
toAnf :: LamExpr -> [Int] -> Int -> LamExpr
toAnf expr [] counter = expr 
toAnf expr (oldNum : oldNums) counter = toAnf (changeAbs expr oldNum counter) oldNums (counter + 1)

toAnf' :: LamExpr -> LamExpr
toAnf' expr = toAnf expr (nub (getLamAbs expr)) 0 




lamExprToString :: LamExpr -> [Char]
lamExprToString (LamVar n) = "x" ++ show n 
lamExprToString (LamAbs n x) = "\\x" ++ show n ++ "->" ++ addToLamAbs x 
lamExprToString (LamApp (LamVar n) y) = lamExprToString (LamVar n) ++ lamExprToString y
lamExprToString (LamApp x y) = addToLamApp x y 

addToLamAbs :: LamExpr -> String 
addToLamAbs x = lamExprToString x 

addToLamApp :: LamExpr -> LamExpr -> String
addToLamApp x y = "(" ++ lamExprToString x ++ ")" ++ lamExprToString y 


prettyPrint :: LamExpr -> String
prettyPrint expr = lamExprToString (toAnf' expr)
