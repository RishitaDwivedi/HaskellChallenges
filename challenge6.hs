import Data.Char
import Parsing
import Control.Monad
import Data.List hiding (splitAt)
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import Control.Monad.STM (check)

data ArithExpr = Add ArithExpr ArithExpr | Mul ArithExpr ArithExpr | Section ArithExpr | SecApp ArithExpr ArithExpr | ArithNum Int deriving (Show, Eq,Read)
data LamExpr = LamApp LamExpr LamExpr | LamAbs Int LamExpr | LamVar Int deriving (Show,Eq,Read)

innerArithRedn1 :: ArithExpr -> Maybe ArithExpr
innerArithRedn1 expr = case expr of
    Add expr1 expr2 -> case expr1 of
        ArithNum n1 -> case expr2 of
            ArithNum n2 -> Just (ArithNum (n1 + n2))
            _ -> fmap (Add expr1) (innerArithRedn1 expr2)
        _ -> fmap (flip Add expr2) (innerArithRedn1 expr1)

    Mul expr1 expr2 -> case expr1 of
        ArithNum n1 -> case expr2 of
            ArithNum n2 -> Just (ArithNum (n1 * n2))
            _ ->  fmap (Mul expr1) (innerArithRedn1 expr2)
        _ -> fmap (flip Mul expr2) (innerArithRedn1 expr1)

    Section expr1 -> innerArithRedn1 expr1

    SecApp expr1 expr2 -> case expr1 of
        Section (ArithNum n1) -> case expr2 of
            ArithNum n2 -> Just (ArithNum (n1 + n2))
            _ -> fmap (Add expr1) (innerArithRedn1 expr2)
        _ -> fmap (flip Add expr2) (innerArithRedn1 expr1)

    _ -> Nothing

-- This function does a beta reduction
innerRedn1 :: LamExpr -> Maybe LamExpr
innerRedn1 expr = case expr of
    LamApp (LamAbs c expr1) expr2 -> return (reduction c expr2 expr1)
    
    LamApp expr1 expr2 -> fmap (LamApp expr1) (innerRedn1 expr2) <|> fmap (flip LamApp expr2) (innerRedn1 expr1)

    LamAbs n x -> fmap (LamAbs n) (innerRedn1 x)

    LamVar _ -> Nothing

reduction :: Int -> LamExpr -> LamExpr -> LamExpr
reduction z x expr = case expr of
    (LamApp expr' expr'') -> LamApp (reduction z x expr') (reduction z x expr'')
    (LamAbs n expr') -> LamAbs n (reduction z x expr')
    (LamVar expr') | z == expr' -> x
                   | otherwise -> LamVar expr'

compareArithLam :: ArithExpr -> (Int,Int)
compareArithLam expr = (arithReductionCounter 0 expr , betaReduceCounter 0 (churchEnc expr))

betaReduceCounter :: Int -> LamExpr -> Int
betaReduceCounter c expr | innerRedn1 expr == Nothing = c 
                         | otherwise = betaReduceCounter (c + 1) (myFromJust (innerRedn1 expr))

arithReductionCounter :: Int -> ArithExpr -> Int
arithReductionCounter c expr | innerArithRedn1 expr == Nothing = c 
                             | otherwise = arithReductionCounter (c + 1) (myFromJust (innerArithRedn1 expr))

myFromJust :: Maybe a -> a
myFromJust (Just x) = x
myFromJust Nothing = error "myFromJust: Nothing"

