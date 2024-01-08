import Data.Char
import Parsing
import Control.Monad
import Data.List hiding (splitAt)
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import Control.Monad.STM (check)

data ArithExpr = Add ArithExpr ArithExpr | Mul ArithExpr ArithExpr | Section ArithExpr | SecApp ArithExpr ArithExpr | ArithNum Int deriving (Show, Eq,Read)
data LamExpr = LamApp LamExpr LamExpr | LamAbs Int LamExpr | LamVar Int deriving (Show,Eq,Read)

--This function takes a number and returns a lamda expression using church encoding on that number  
numToLam :: Int -> LamExpr
numToLam n = LamAbs 0 (LamAbs 1 (numNew n 0 1))

numNew :: Int -> Int -> Int -> LamExpr
numNew 0 _ _ = LamVar 1
numNew n a b = LamApp (LamVar 0) (numNew (n-1) a b)

--This function will add two church encoded expression 
addLamda :: LamExpr -> LamExpr -> LamExpr
addLamda x y = LamApp (LamApp (encode) (x)) (y) 
               where 
                encode = LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (LamVar 2)) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3))))))
--This function will multiply two church encoded numbers
--check if this is correct it might be creating errors  
multLamda :: LamExpr -> LamExpr -> LamExpr
multLamda x y = LamApp (LamApp (encode) (x)) (y) 
                where 
                    encode = LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (LamApp (LamVar 1) (LamVar 2))) (LamVar 3)))))

--This function joins it all together 
churchEnc :: ArithExpr -> LamExpr
churchEnc (ArithNum n) = numToLam n
churchEnc (Add x y) = addLamda (churchEnc x) (churchEnc y)
churchEnc (Mul x y) = multLamda (churchEnc x) (churchEnc y)
churchEnc (SecApp x y) = addLamda (churchEnc x) (churchEnc y)
churchEnc (Section x) = churchEnc x 

