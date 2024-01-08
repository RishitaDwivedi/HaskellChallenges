import Data.Char
import Parsing
import Control.Monad
import Data.List hiding (splitAt)
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import Control.Monad.STM (check)

data ArithExpr = Add ArithExpr ArithExpr | Mul ArithExpr ArithExpr | Section ArithExpr | SecApp ArithExpr ArithExpr | ArithNum Int deriving (Show, Eq,Read)

--this describes expr in the grammar 
expr :: Parser ArithExpr 
expr = do x <- expr1 
          char '*'
          y <- expr 
          return (Mul x y )
       <|> expr1 

--this function describes expr1 in the grammar 
expr1 :: Parser ArithExpr 
expr1 = do x <- expr2 
           char '+' 
           y <- expr1 
           return (Add x y)
        <|> expr2

--this function describes expr2 in the grammar            
expr2 :: Parser ArithExpr
expr2 = num <|> do x <- section
                   y <- expr2 
                   return (SecApp x y )
                <|> do char '('   
                       y <- expr 
                       char ')'
                       return y --need to figure out what this would be 

--this function describes Section in the grammar 
section :: Parser ArithExpr
section = do char '('
             char '+'
             x <- expr 
             char ')'
             return(Section x)

--this function describes Num 
num :: Parser ArithExpr
num = do x <- some digit' 
         return (ArithNum (read x))


--this function describes digits 
digit' :: Parser Char
digit' = char '0'
         <|> char '1'
         <|> char '2'
         <|> char '3'
         <|> char '4'
         <|> char '5'
         <|> char '6'
         <|> char '7'
         <|> char '8'
         <|> char '9'


filterSpace :: String -> String
filterSpace tofilter = filter (not . isSpace) tofilter

--This function puts it all together to parseArith 
--Right now it only returns Nothing for some reason 
--figure out whats wrong with it 
parseArith :: String -> Maybe ArithExpr
parseArith str = case parse expr (filterSpace str) of
    [] -> Nothing
    [(result, "")] -> Just result
    _ -> Nothing 
       
 
