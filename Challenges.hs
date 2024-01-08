{-# LANGUAGE DeriveGeneric #-}
-- comp2209 Functional Programming Challenges
-- (c) University of Southampton 2022
-- Skeleton code to be updated with your solutions
-- The dummy functions here simply return an arbitrary value that is usually wrong 

-- DO NOT MODIFY THE FOLLOWING LINES OF CODE
module Challenges (Atoms,Interactions,Pos,EdgePos,Side(..),Marking(..),
                   LamExpr(..),ArithExpr(..),
                   calcBBInteractions,solveBB,prettyPrint,
                   parseArith,churchEnc,innerRedn1,innerArithRedn1,compareArithLam) where


-- Import standard library and parsing definitions from Hutton 2016, Chapter 13
import Data.Char
import Parsing
import Control.Monad
import Data.List
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq


instance NFData LamExpr
instance NFData ArithExpr 
instance NFData Marking 
instance NFData Side 
-- Challenge 1
-- Calculate Interactions in the Black Box

type Atoms = [ Pos ]
type Interactions = [  ( EdgePos , Marking )  ] 
type Pos = (Int, Int)   -- top left is (1,1) , bottom right is (N,N) where N is size of grid
type EdgePos = ( Side , Int ) -- int range is 1 to N where N is size of grid

data Side = North | East | South | West
            deriving (Show, Eq, Ord, Generic)

data Marking =  Absorb | Reflect | Path EdgePos
                deriving (Show, Eq)

type Interaction = (EdgePos , Marking)
--this type keeps original position and (Side,Pos) have the side its moving towards and the current position 
--type cuurentPos = ((Side , Int) , (Side , (Int , Int)))
type CurrentPos = (EdgePos , (Side, Pos))  


--This function uses a list comprehension to calculate interations one ray at a time with the help of two locally defined functions 
--the functions defined locally are - solve and solve'
--solve uses gaurd statements to check the rays markings and then move it one step at a time 
--solve' also checks if the ray is at the end position as well as everything solve does 
calcBBInteractions :: Int -> Atoms -> [EdgePos] -> Interactions
calcBBInteractions n atoms rays = [solve n atoms (createCurrentPos n ray)| ray <- rays ] 
    where 
        solve :: Int -> Atoms -> CurrentPos -> Interaction 
        solve n atoms pos | checkEdgeReflection n (fst pos) atoms = (fst pos , Reflect)
                          | checkAbsorb pos atoms = (fst pos , Absorb) 
                          | checkDeflect pos atoms = solve' n atoms (deflect pos atoms) 
                          | otherwise = solve' n atoms (moveOne pos)
                            
        solve' ::  Int -> Atoms -> CurrentPos -> Interaction 
        solve' n atoms pos | checkEdgeReflection n (fst pos) atoms = (fst pos , Reflect)
                           | checkAbsorb pos atoms = (fst pos , Absorb) 
                           | checkDeflect pos atoms = solve' n atoms (deflect pos atoms) 
                           | checkEndPos pos n = (fst pos , Path(pathFind pos))
                           | otherwise = solve' n atoms (moveOne pos)                    

--this function returns all positions of coordinates that would absorb a ray 
absorptionCoords :: Atoms -> Atoms 
absorptionCoords [] = []
absorptionCoords (atomcoord : atomcoords) =(atomcoord) : ((fst atomcoord) + 1, snd atomcoord) : ((fst atomcoord) - 1 , snd atomcoord) : (fst atomcoord , (snd atomcoord) + 1) : (fst atomcoord , (snd atomcoord) - 1) : absorptionCoords atomcoords

--This function returns all coordinates at the top right of an atom 
deflectCoordsTopRight :: Atoms -> [Pos]
deflectCoordsTopRight [] = []
deflectCoordsTopRight (deflectTopR : deflectTopRs) = (fst deflectTopR + 1 , snd deflectTopR - 1) : deflectCoordsTopRight deflectTopRs 

--This function returns all coordinates at the bottom right of an atom 
deflectCoordsBottomRight :: Atoms -> [Pos]
deflectCoordsBottomRight [] = []
deflectCoordsBottomRight (deflectBottomR : deflectBottomRs) = (fst deflectBottomR + 1 , snd deflectBottomR + 1) : deflectCoordsBottomRight deflectBottomRs 

--This function returns all coordinates at the top left of an atom 
deflectCoordsTopLeft :: Atoms -> [Pos]
deflectCoordsTopLeft [] = []
deflectCoordsTopLeft (deflectTopL : deflectTopLs) = (fst deflectTopL - 1 , snd deflectTopL - 1) : deflectCoordsTopLeft deflectTopLs 

--This function returns all coordinates at the bottom left of an atom 
deflectCoordsBottomLeft :: Atoms -> [Pos]
deflectCoordsBottomLeft [] = []
deflectCoordsBottomLeft (deflectBottomL : deflectBottomLs) = (fst deflectBottomL - 1 , snd deflectBottomL + 1) : deflectCoordsBottomLeft deflectBottomLs 


--This function will return True if there is an edge reflection 
checkEdgeReflection :: Int -> EdgePos -> Atoms -> Bool
checkEdgeReflection n ray atoms | fst ray == North = intersect [(snd ray + 1,1) , (snd ray - 1,1)] atoms /= []
                                | fst ray == East = intersect [(n , snd ray + 1) , (n , snd ray - 1)] atoms /= []
                                | fst ray == South = intersect [(snd ray + 1 , n) , (snd ray - 1 , n)] atoms /= []
                                | fst ray == West = intersect [(1, snd ray + 1) , (1 , snd ray - 1)] atoms /= []
                                | otherwise = False


--This function is to move a ray one block 
moveOne :: CurrentPos -> CurrentPos
moveOne currentPos | fst(snd currentPos) == North = (fst currentPos , (fst (snd currentPos) , (fst(snd(snd currentPos)) , snd(snd(snd currentPos)) - 1)))
                   | fst(snd currentPos) == East = (fst currentPos , (fst (snd currentPos) , (fst(snd(snd currentPos)) + 1 , snd(snd(snd currentPos)))))
                   | fst(snd currentPos) == South = (fst currentPos , (fst (snd currentPos) , (fst(snd(snd currentPos)) , snd(snd(snd currentPos)) + 1)))
                   | fst(snd currentPos) == West = (fst currentPos , (fst (snd currentPos) , (fst(snd(snd currentPos)) - 1 , snd(snd(snd currentPos)))))


--This fucntion will check whether current position is an absorption block or not 
checkAbsorb :: CurrentPos -> Atoms -> Bool 
checkAbsorb currentPos atoms | intersect [snd(snd currentPos)] (absorptionCoords atoms) /= [] = True 
                             | otherwise = False 

--This function returns True if an ray needs to be deflected
checkDeflect :: CurrentPos -> Atoms -> Bool
checkDeflect currentPos atoms    | intersect [snd(snd currentPos)] (deflectCoordsBottomRight atoms) /= [] && (fst(snd currentPos) == North) = True    
                                 | intersect [snd(snd currentPos)] (deflectCoordsBottomRight atoms) /= [] && (fst(snd currentPos) == West) = True 
                                 | intersect [snd(snd currentPos)] (deflectCoordsBottomLeft atoms) /= [] && (fst(snd currentPos) == North) = True 
                                 | intersect [snd(snd currentPos)] (deflectCoordsBottomLeft atoms) /= [] && (fst(snd currentPos) == East) = True  
                                 | intersect [snd(snd currentPos)] (deflectCoordsTopRight atoms) /= [] && (fst(snd currentPos) == South) = True 
                                 | intersect [snd(snd currentPos)] (deflectCoordsTopRight atoms) /= [] && (fst(snd currentPos) == West) = True 
                                 | intersect [snd(snd currentPos)] (deflectCoordsTopLeft atoms) /= [] && (fst(snd currentPos) == East) = True  
                                 | intersect [snd(snd currentPos)] (deflectCoordsTopLeft atoms) /= [] && (fst(snd currentPos) == South) = True 
                                 | otherwise = False                                                        

--This function deflects a ray and returns its new position depending on :
--the direction the ray was heading and if it was at the top right of the atom or the top left , bottom right or bottom left 
deflect :: CurrentPos -> Atoms -> CurrentPos
deflect currentPos atoms | intersect [snd(snd currentPos)] (deflectCoordsBottomRight atoms) /= [] && (fst(snd currentPos) == North) = (fst currentPos , (East , snd(snd currentPos)))    
                         | intersect [snd(snd currentPos)] (deflectCoordsBottomRight atoms) /= [] && (fst(snd currentPos) == West) = (fst currentPos , (South , snd(snd currentPos)))    
                         | intersect [snd(snd currentPos)] (deflectCoordsBottomLeft atoms) /= [] && (fst(snd currentPos) == North) = (fst currentPos , (West , snd(snd currentPos)))   
                         | intersect [snd(snd currentPos)] (deflectCoordsBottomLeft atoms) /= [] && (fst(snd currentPos) == East) = (fst currentPos , (South , snd(snd currentPos)))    
                         | intersect [snd(snd currentPos)] (deflectCoordsTopRight atoms) /= [] && (fst(snd currentPos) == South) = (fst currentPos , (East , snd(snd currentPos)))   
                         | intersect [snd(snd currentPos)] (deflectCoordsTopRight atoms) /= [] && (fst(snd currentPos) == West) = (fst currentPos , (North , snd(snd currentPos)))    
                         | intersect [snd(snd currentPos)] (deflectCoordsTopLeft atoms) /= [] && (fst(snd currentPos) == East) = (fst currentPos , (North , snd(snd currentPos)))    
                         | intersect [snd(snd currentPos)] (deflectCoordsTopLeft atoms) /= [] && (fst(snd currentPos) == South) = (fst currentPos , (West , snd(snd currentPos)))    

--This function checks whether you're in the end position or not 
--end position = at the edge 
checkEndPos :: CurrentPos -> Int -> Bool 
checkEndPos currentPos n | fst(snd(snd currentPos)) == 0 || snd(snd(snd currentPos)) == 0 || fst(snd(snd currentPos)) == (n + 1) || snd(snd(snd currentPos)) == (n + 1)  = True
                         | otherwise = False 

--This function checks if the ray was reflected or not using the current postion and the rays EdgePos 
checkReflect :: Int -> CurrentPos -> Bool  
checkReflect n pos | fst(fst pos) == North && snd(snd pos) == (snd (fst pos) , 0) = True
                   | fst(fst pos) == East && snd(snd pos) == (n + 1 , snd(fst pos)) = True 
                   | fst(fst pos) == South && snd(snd pos) == (snd(fst pos) , n + 1) = True
                   | fst(fst pos) == West && snd(snd pos) == (0 , snd(fst pos)) = True
                   | otherwise = False 

--This function returns the coordinates if the marking == Path 
pathFind :: CurrentPos -> EdgePos
pathFind pos | fst(snd pos) == North = (North , fst(snd(snd pos)))
             | fst(snd pos) == East = (East , snd(snd(snd pos)))
             | fst(snd pos) == South = (South , fst(snd(snd pos)))
             | fst(snd pos) == West = (West , snd(snd(snd pos)))

--This function returns the initial CurrentPos of a ray
createCurrentPos ::Int -> EdgePos -> CurrentPos
createCurrentPos n ray | fst ray == North = (ray , (South, (snd ray , 0)))    
                       | fst ray == East =  (ray  , (West, (n + 1 , snd ray)))    
                       | fst ray == South = (ray , (North, (snd ray , n + 1))) 
                       | fst ray == West = (ray , (East, (0 , snd ray)))    
                               
            

-- Challenge 2
-- Find atoms in a Black Box

solveBB :: Int -> Interactions -> Atoms 
solveBB grid interactions = minList (possibleAtms grid interactions)

-- making all the possible atoms

flatten :: [[a]] -> [a]
flatten [] = []
flatten (x:xs) = x ++ flatten xs

minList :: [[a]] -> [a]
minList [] = []
minList (x:xs) = foldl (\acc y -> if length y < length acc then y else acc) x xs

possibleAtms :: Int -> Interactions -> [Atoms]
possibleAtms grid interactions = [atoms | atoms <- allSubsets , isSolution grid [atoms] interactions]
 where
  allSubsets = subsets [atoms | interaction <- interactions , atoms <- (atomsFromInteraction grid interaction)]

isSolution :: Int -> [Atoms] -> Interactions -> Bool
isSolution grid atmss interactions = foldr (&&) True [calcBBInteractions grid atms edgePos == interactions | atms <- atmss]
 where
  edgePos = map fst interactions

subsets :: [[a]] -> [[a]]
subsets [] = [[]]
subsets (x:xs) = 
  let subs = subsets xs
  in subs ++ map (x ++) subs

------------------------------------------------------------------------------------------------------------------

atomsFromInteraction :: Int -> ( EdgePos , Marking ) -> [Atoms]
atomsFromInteraction grid interactions = case interactions of
    ((sideEntry , entry) , Absorb) -> absorbs grid (sideEntry , entry)

    ((sideEntry , entry) , Reflect) -> reflects grid (sideEntry , entry) 

    ((North , entry) , Path (North , exit)) -> sameSideDeflects grid North (entry, exit)
    ((South , entry) , Path (South , exit)) -> sameSideDeflects grid South (entry, exit)
    ((East , entry) , Path (East , exit)) -> sameSideDeflects grid East (entry, exit)
    ((West , entry) , Path (West , exit)) -> sameSideDeflects grid West (entry, exit)
 
    ((North , entry) , Path (South , exit)) -> oppositeSideDeflects grid (North , South) (entry, exit)
    ((South , entry) , Path (North , exit)) -> oppositeSideDeflects grid (South , North) (entry, exit)
    ((East , entry) , Path (West , exit)) -> oppositeSideDeflects grid (East , West) (entry, exit)
    ((West , entry) , Path (East , exit)) -> oppositeSideDeflects grid (West , East) (entry, exit)

    ((sideEntry , entry) , Path (sideExit , exit)) -> sideDelects grid (sideEntry , sideExit) (entry , exit)

absorbs :: Int -> EdgePos -> [Atoms]
absorbs grid tps = case tps of
    (North , entry) -> [[(entry , y)] | y <- [1..grid]]
    (South , entry) -> [[(entry , y)] | y <- [1..grid]]
    (East , entry) -> [[(x , entry)] | x <- [1..grid]] 
    (West , entry) -> [[(x , entry)] | x <- [1..grid]]

reflects :: Int -> EdgePos -> [Atoms]
reflects grid tps = case tps of
    (North , entry) -> [(entry - 1 , 1)] : [(entry + 1 , 1)] : [(entry - 1 , y) | y <- [2..grid]] : [(entry + 1 , y) | y <- [2..grid]] : []
    (South , entry) -> [(entry - 1 , grid)] : [(entry + 1 , grid)] : [(entry - 1 , y) | y <- [2..grid]] : [(entry + 1 , y) | y <- [2..grid]] : []
    (East , entry) -> [(grid , entry - 1)] : [(grid , entry + 1)] : [(x , entry - 1) | x <- [2..grid]] : [(x , entry + 1) | x <- [2..grid]] : []
    (West , entry) -> [(1 , entry - 1)] : [(1 , entry + 1)] : [(x , entry - 1) | x <- [2..grid]] : [(x , entry + 1) | x <- [2..grid]] : []

sameSideDeflects :: Int -> Side -> (Int , Int) -> [Atoms]
sameSideDeflects grid side (entry , exit) = case side of
    North | entry >= exit -> [[(exit - 1 , y) , (entry + 1 , y)] | y <- [2..grid]]
          | otherwise -> [[(entry - 1 , y) , (exit + 1 , y)] | y <- [2..grid]]

    South | entry >= exit -> [[(exit - 1 , y) , (entry + 1 , y)] | y <- [1..(grid - 1)]]
          | otherwise -> [[(entry - 1 , y) , (exit + 1 , y)] | y <- [1..(grid - 1)]]

    West | entry >= exit -> [[(y , exit - 1) , (y , entry + 1)] | y <- [2..grid]]
         | otherwise -> [[(y , entry - 1) , (y , exit + 1)] | y <- [2..grid]]

    East | entry >= exit -> [[(y , exit - 1) , (y , entry + 1)] | y <- [1..(grid - 1)]]
         | otherwise -> [[(y , entry - 1) , (y , exit + 1)] | y <- [1..(grid - 1)]]


oppositeSideDeflects :: Int -> (Side , Side) -> (Int , Int) -> [Atoms]
oppositeSideDeflects grid (entrySide , exitSide) (entry , exit) = case entrySide of
    North | entrySide > exitSide -> [[(exit - 1 , z) , (entry + 1 , z)] | z <- [1..grid]]
          | otherwise -> [[(entry - 1 , z) , (exit + 1 , z)] | z <- [1..grid]]

    South | entrySide > exitSide -> [[(exit - 1 , z) , (entry + 1 , z)] | z <- [1..grid]]
          | otherwise -> [[(entry - 1 , z) , (exit + 1 , z)] | z <- [1..grid]]

    East | entrySide > exitSide -> [[(z , exit - 1) , (z , entry + 1)] | z <- [1..grid]]
         | otherwise -> [[(z , entry - 1) , (z , exit + 1)] | z <- [1..grid]] 

    West | entrySide > exitSide -> [[(z , exit - 1) , (z , entry + 1)] | z <- [1..grid]]
         | otherwise -> [[(z , entry - 1) , (z , exit + 1)] | z <- [1..grid]] 

sideDelects :: Int -> (Side , Side) -> (Int , Int) -> [Atoms]
sideDelects grid (sideEntry , sideExit) (entry , exit) = case (sideEntry , sideExit) of
    (North , x) | x == East -> [(entry - 1 , exit + 1)] : []
                | x == West -> [(entry + 1 , exit + 1)] : []

    (South , x) | x == East -> [(entry - 1 , exit - 1)] : []
                | x == West -> [(entry + 1 , exit - 1)] : []

    (East , x) | x == North -> [(entry - 1 , exit + 1)] : []
               | x == South -> [(entry - 1 , exit - 1)] : []

    (West , x) | x == North -> [(entry + 1 , exit + 1)] : []
               | x == South -> [(entry + 1 , exit - 1)] : []




-- Challenge 3
-- Pretty Printing Lambda with Alpha-Normalisation 

data LamExpr =  LamApp LamExpr LamExpr  |  LamAbs Int LamExpr  |  LamVar Int 
                deriving (Eq, Show, Read)

--This function returns a string representation of an alpha - equivalent expression given in Alpha-Normal form
--it uses the function toAnf'
prettyPrint :: LamExpr -> String
prettyPrint expr = lamExprToString (toAnf' expr)

--This function returns Alpha normal form of an expression when using the input of getLamAbs and 0 
changeAbs :: LamExpr -> Int -> Int -> LamExpr
changeAbs (LamApp x y) oldN newN = LamApp (changeAbs x oldN newN) (changeAbs y oldN newN)
changeAbs ((LamAbs n x)) oldN newN | n == oldN = (LamAbs newN (changeAbs (changeVars x oldN newN) oldN newN)) 
                                   | otherwise = (LamAbs n) (changeAbs x oldN newN)
changeAbs ((LamVar n)) oldN newN  = LamVar n                                

--this function will go into the LamAbs and check if the variables n == oldN 
--it changes bound variables 
changeVars :: LamExpr -> Int -> Int -> LamExpr
changeVars (LamApp x y) oldN newN = LamApp (changeVars x oldN newN) (changeVars y oldN newN)
changeVars (LamAbs n x) oldN newN = LamAbs n (changeVars x oldN newN )
changeVars (LamVar n) oldN newN | n == oldN = LamVar newN 
                                | otherwise = LamVar n  

--This function will return an expression in its alpha normal form using the function changeAbs
-- it will take a list of ints --> the old abs numbers 
--and a counter 
toAnf :: LamExpr -> [Int] -> Int -> LamExpr
toAnf expr [] counter = expr 
toAnf expr (oldNum : oldNums) counter = toAnf (changeAbs expr oldNum counter) oldNums (counter + 1)

--This function cleans up the code of toAnf 
toAnf' :: LamExpr -> LamExpr
toAnf' expr = toAnf expr (nub (getLamAbs expr)) 0 

--This function returns the list of integers that are attached to LamAbs and LamVar 
getLamAbs :: LamExpr -> [Int]
getLamAbs (LamApp x y) = getLamAbs x ++ getLamAbs y 
getLamAbs (LamAbs n x) = n : getLamAbs x 
getLamAbs (LamVar n) = []

--This function parses a a LamExpr to a string 
lamExprToString :: LamExpr -> [Char]
lamExprToString (LamVar n) = "x" ++ show n 
lamExprToString (LamAbs n x) = "\\x" ++ show n ++ "->" ++ addToLamAbs x 
lamExprToString (LamApp (LamVar n) y) = lamExprToString (LamVar n) ++ lamExprToString y
lamExprToString (LamApp x y) = addToLamApp x y 

addToLamAbs :: LamExpr -> String 
addToLamAbs x = lamExprToString x 

addToLamApp :: LamExpr -> LamExpr -> String
addToLamApp x y = "(" ++ lamExprToString x ++ ")" ++ lamExprToString y 



-- Challenge 4 
-- Parsing Arithmetic Expressions

data ArithExpr = Add ArithExpr ArithExpr | Mul ArithExpr ArithExpr 
               | Section ArithExpr  | SecApp ArithExpr ArithExpr | ArithNum Int
    deriving (Show,Eq,Read) 

--This function returns Nothing or an Abstarct Syntax tree in accordance with the grammar given 
parseArith :: String -> Maybe ArithExpr
parseArith str = case parse expr (filterSpace str) of
    [] -> Nothing
    [(result, "")] -> Just result
    _ -> Nothing 

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

--this function describes Num in the grammar 
num :: Parser ArithExpr
num = do x <- some digit' 
         return (ArithNum (read x))


--this function describes digits in the grammar 
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

--This function filters out spaces
filterSpace :: String -> String
filterSpace tofilter = filter (not . isSpace) tofilter


-- Challenge 5
-- Church Encoding of arithmetic

--This function joins it all together 
churchEnc :: ArithExpr -> LamExpr
churchEnc (ArithNum n) = numToLam n
churchEnc (Add x y) = addLamda (churchEnc x) (churchEnc y)
churchEnc (Mul x y) = multLamda (churchEnc x) (churchEnc y)
churchEnc (SecApp x y) = addLamda (churchEnc x) (churchEnc y)
churchEnc (Section x) = churchEnc x 

--This function takes a number and returns a lamda expression using church encoding on that number  
numToLam :: Int -> LamExpr
numToLam n = LamAbs 0 (LamAbs 1 (numNew n 0 1))

numNew :: Int -> Int -> Int -> LamExpr
numNew 0 _ _ = LamVar 1
numNew n a b = LamApp (LamVar 0) (numNew (n-1) a b)

--This function will return the addition of two church encoded expression 
addLamda :: LamExpr -> LamExpr -> LamExpr
addLamda x y = LamApp (LamApp (encode) (x)) (y) 
               where 
                encode = LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (LamVar 2)) (LamApp (LamApp (LamVar 1) (LamVar 2)) (LamVar 3))))))

--This function will return the multication of two church encoded numbers  
multLamda :: LamExpr -> LamExpr -> LamExpr
multLamda x y = LamApp (LamApp (encode) (x)) (y) 
                where 
                    encode = LamAbs 0 (LamAbs 1 (LamAbs 2 (LamAbs 3 (LamApp (LamApp (LamVar 0) (LamApp (LamVar 1) (LamVar 2))) (LamVar 3)))))



-- Challenge 6
-- Compare Innermost Reduction for Arithmetic and its Church Encoding

-- This function does an arithmetic reduction
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

