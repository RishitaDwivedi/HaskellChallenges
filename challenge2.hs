import Data.Char
import Parsing
import Control.Monad
import Data.List hiding (splitAt)
import GHC.Generics (Generic,Generic1)
import Control.DeepSeq
import Control.Monad.STM (check)

type Atoms = [ Pos ]
type Interactions = [  ( EdgePos , Marking )  ] 
type Interaction = (EdgePos , Marking)
type Pos = (Int, Int)   -- top left is (1,1) , bottom right is (N,N) where N is size of grid
type EdgePos = ( Side , Int ) -- int range is 1 to N where N is size of grid

data Side = North | East | South | West
            deriving (Show, Eq, Ord, Generic)

data Marking =  Absorb | Reflect | Path EdgePos
                deriving (Show, Eq)

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



--delete this when copying 
type CurrentPos = (EdgePos , (Side, Pos))  

--this function returns all positions of coords that would absorb a ray 
absorptionCoords :: Atoms -> Atoms 
absorptionCoords [] = []
absorptionCoords (atomcoord : atomcoords) =(atomcoord) : ((fst atomcoord) + 1, snd atomcoord) : ((fst atomcoord) - 1 , snd atomcoord) : (fst atomcoord , (snd atomcoord) + 1) : (fst atomcoord , (snd atomcoord) - 1) : absorptionCoords atomcoords

deflectCoordsTopRight :: Atoms -> [Pos]
deflectCoordsTopRight [] = []
deflectCoordsTopRight (deflectTopR : deflectTopRs) = (fst deflectTopR + 1 , snd deflectTopR - 1) : deflectCoordsTopRight deflectTopRs 

deflectCoordsBottomRight :: Atoms -> [Pos]
deflectCoordsBottomRight [] = []
deflectCoordsBottomRight (deflectBottomR : deflectBottomRs) = (fst deflectBottomR + 1 , snd deflectBottomR + 1) : deflectCoordsBottomRight deflectBottomRs 

deflectCoordsTopLeft :: Atoms -> [Pos]
deflectCoordsTopLeft [] = []
deflectCoordsTopLeft (deflectTopL : deflectTopLs) = (fst deflectTopL - 1 , snd deflectTopL - 1) : deflectCoordsTopLeft deflectTopLs 

deflectCoordsBottomLeft :: Atoms -> [Pos]
deflectCoordsBottomLeft [] = []
deflectCoordsBottomLeft (deflectBottomL : deflectBottomLs) = (fst deflectBottomL - 1 , snd deflectBottomL + 1) : deflectCoordsBottomLeft deflectBottomLs 


--This function will check whether there is an edge reflection 
checkEdgeReflection :: Int -> EdgePos -> Atoms -> Bool
checkEdgeReflection n ray atoms | fst ray == North = intersect [(snd ray + 1,1) , (snd ray - 1,1)] atoms /= []
                                | fst ray == East = intersect [(n , snd ray + 1) , (n , snd ray - 1)] atoms /= []
                                | fst ray == South = intersect [(snd ray + 1 , n) , (snd ray - 1 , n)] atoms /= []
                                | fst ray == West = intersect [(1, snd ray + 1) , (1 , snd ray - 1)] atoms /= []
                                | otherwise = False


--This function is to move one block 
moveOne :: CurrentPos -> CurrentPos
moveOne currentPos | fst(snd currentPos) == North = (fst currentPos , (fst (snd currentPos) , (fst(snd(snd currentPos)) , snd(snd(snd currentPos)) - 1)))
                   | fst(snd currentPos) == East = (fst currentPos , (fst (snd currentPos) , (fst(snd(snd currentPos)) + 1 , snd(snd(snd currentPos)))))
                   | fst(snd currentPos) == South = (fst currentPos , (fst (snd currentPos) , (fst(snd(snd currentPos)) , snd(snd(snd currentPos)) + 1)))
                   | fst(snd currentPos) == West = (fst currentPos , (fst (snd currentPos) , (fst(snd(snd currentPos)) - 1 , snd(snd(snd currentPos)))))


--This fucntion will check whether current position is an absorption block or not 
checkAbsorb :: CurrentPos -> Atoms -> Bool 
checkAbsorb currentPos atoms | intersect [snd(snd currentPos)] (absorptionCoords atoms) /= [] = True 
                             | otherwise = False 

--This function checks for deflection and if it is it changes its direction accordingly 
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
checkEndPos :: CurrentPos -> Int -> Bool 
checkEndPos currentPos n | fst(snd(snd currentPos)) == 0 || snd(snd(snd currentPos)) == 0 || fst(snd(snd currentPos)) == (n + 1) || snd(snd(snd currentPos)) == (n + 1)  = True
                         | otherwise = False 

--This function checks if it was reflected or not 
checkReflect :: Int -> CurrentPos -> Bool  
checkReflect n pos | fst(fst pos) == North && snd(snd pos) == (snd (fst pos) , 0) = True
                   | fst(fst pos) == East && snd(snd pos) == (n + 1 , snd(fst pos)) = True 
                   | fst(fst pos) == South && snd(snd pos) == (snd(fst pos) , n + 1) = True
                   | fst(fst pos) == West && snd(snd pos) == (0 , snd(fst pos)) = True
                   | otherwise = False 


pathFind :: CurrentPos -> EdgePos
pathFind pos | fst(snd pos) == North = (North , fst(snd(snd pos)))
             | fst(snd pos) == East = (East , snd(snd(snd pos)))
             | fst(snd pos) == South = (South , fst(snd(snd pos)))
             | fst(snd pos) == West = (West , snd(snd(snd pos)))

createCurrentPos ::Int -> EdgePos -> CurrentPos
createCurrentPos n ray | fst ray == North = (ray , (South, (snd ray , 0)))    
                       | fst ray == East =  (ray  , (West, (n + 1 , snd ray)))    
                       | fst ray == South = (ray , (North, (snd ray , n + 1))) 
                       | fst ray == West = (ray , (East, (0 , snd ray)))    
                               
--This is the final function that will return all interactions 
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

                                                       
                        
 
                                                       
                         
                        






    