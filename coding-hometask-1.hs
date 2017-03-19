import Data.List ((\\))


-- Решение задачи для произвольных весов
canPack :: (Num a, Ord a) => [a] -> [a] -> [a] -> Bool
canPack _ [] _ = True
canPack [] _ []  = False
canPack (b:bs) (w:ws) skipb = ((b >= w) && canPack (b - w : (bs ++ skipb)) ws [])
                              || not (null bs) && canPack bs (w:ws) (b:skipb)

solveBpDecision :: [Rational] -> Int -> Bool
solveBpDecision weights binNumber = canPack (replicate binNumber 1) weights []

binarySearch :: Integral a => (a -> Bool) -> a -> a -> Maybe a
binarySearch f left right
    | left > right  = Nothing
    | left == right = if f left == True then Just left else Nothing
    | f mid == True    = binarySearch f left mid
    | otherwise     = binarySearch f (mid + 1) right
    where mid       = (left + right - 1) `div` 2

solveBpEvaluation :: [Rational] -> Maybe Int
solveBpEvaluation weights = binarySearch (solveBpDecision weights) 0 (length weights)

-- Заполнение одной корзины при оптимальном решении
fillOneBin :: [Rational] -> [Rational] -> [Rational]
fillOneBin bin [] = bin
fillOneBin bin (w:ws)
    | binNumber == Nothing                                = []
    | (solveBpEvaluation ((sum bin + w):ws)) == binNumber = fillOneBin (w:bin) ws
    | otherwise                                           = fillOneBin bin ws
    where binNumber = solveBpEvaluation ((sum bin):(w:ws)) 

fillAllBins :: [[Rational]] -> [Rational] -> [[Rational]]
fillAllBins bins [] = bins
fillAllBins bins weights 
        | bin == [] = []
        | otherwise = fillAllBins (bin:bins) (weights \\ bin)
        where bin = fillOneBin [] weights

solveBpSearch :: [Rational] -> [[Rational]]
solveBpSearch weights = fillAllBins [] weights
