import Data.List (elemIndex, (\\))
import Data.Maybe (fromJust)


canPack :: (Num a, Ord a) => [a] -> [a] -> [a] -> Bool
canPack _ [] [] = True
canPack [] _ _ = False
canPack _ [] _ = False
canPack (b:bs) (w:ws) bw = ((b >= w) && canPack (b - w : bs) (bw ++ ws) [])
                           || canPack (b:bs) ws (w:bw)
                           || canPack bs (bw ++ (w:ws)) []

solveBpDecision :: (Num a, Ord a) => Int -> [a] -> Bool
solveBpDecision binNumber weights = canPack (replicate binNumber 1) weights []

solveBpEvaluation :: (Num a, Ord a) => [a] -> Maybe Int
solveBpEvaluation weights = elemIndex True $ map (flip solveBpDecision weights) [0,1..length weights]

fillOneBin :: (Num a, Ord a) => [a] -> [a] -> [a]
fillOneBin bin [] = bin
fillOneBin bin (w:ws)
    | binNumber == Nothing = []
    | (solveBpEvaluation ((sum bin + w):ws)) == binNumber = fillOneBin (w:bin) ws
    | otherwise = fillOneBin bin ws
    where binNumber = solveBpEvaluation ((sum bin):(w:ws)) 

fillAllBins :: (Num a, Ord a) => [[a]] -> [a] -> [[a]]
fillAllBins bins [] = bins
fillAllBins bins weights 
        | bin == [] = []
        | otherwise = fillAllBins (bin:bins) (weights \\ bin)
        where bin = fillOneBin [] weights

solveBpSearch :: (Num a, Ord a) => [a] -> [[a]]
solveBpSearch weights = fillAllBins [] weights
