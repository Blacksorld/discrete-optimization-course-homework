import Data.List (minimumBy, delete, elemIndex)
import Data.Maybe (fromJust, isJust, catMaybes)
import Data.Char (isDigit)
import System.CPUTime 

type Point a = (a, a)

euclidianDistance :: (Floating a) => Point a -> Point a -> a
euclidianDistance (x1, y1) (x2, y2) = sqrt $ (x2 - x1)**2 + (y2 - y1)**2


calculateTourLength :: (Floating a) => [Point a] -> a
calculateTourLength [] = 0
calculateTourLength [_] = 0
calculateTourLength (x:y:xs) = calcylateCycleLength x (x:y:xs)
    where calcylateCycleLength x [y] = euclidianDistance x y
          calcylateCycleLength x (y:z:ys) = euclidianDistance y z + calcylateCycleLength x (z:ys)


nearestPoint :: (Floating a, Ord a) => Point a -> [Point a] -> Point a
nearestPoint x xs = minimumBy (comparator) xs
    where comparator y z = euclidianDistance x y `compare` euclidianDistance x z


solveTspNearestNeighbour :: (Floating a, Ord a) => [Point a] -> [Point a]
solveTspNearestNeighbour [] = []
solveTspNearestNeighbour [x] = [x]
solveTspNearestNeighbour (x:xs) = nearestNeighbour [x] xs
    where nearestNeighbour cycle [] = cycle
          nearestNeighbour (c:cs) xs = nearestNeighbour (nextPoint:c:cs) (delete nextPoint xs)
              where nextPoint = nearestPoint c xs


nearestInsertion :: (Floating a, Ord a) => [(Point a, Point a)] -> [Point a] -> [Point a]
nearestInsertion cycle [] = map fst cycle
nearestInsertion cycle xs = nearestInsertion newCycle (delete nextPoint xs)
    where nextPoint = snd $ minimumBy comparator [(x,y) | (x,_) <- cycle, y <- xs]
              where comparator a b = euclidianDistance (fst a) (snd a) `compare` euclidianDistance (fst b) (snd b)
          minEdge = minimumBy comparator cycle
              where dist x = euclidianDistance (fst x) nextPoint + euclidianDistance nextPoint (snd x) - euclidianDistance (fst x) (snd x)
                    comparator x y = dist x `compare` dist y
          position = fromJust $ elemIndex minEdge cycle
          splittedCycle = splitAt position cycle 
          newCycle = left ++ [(fst mid, nextPoint), (nextPoint, snd mid)] ++ right
              where left = fst splittedCycle 
                    right = tail $ snd splittedCycle 
                    mid = head $ snd splittedCycle


solveTspNearestInsertion :: (Floating a, Ord a) => [Point a] -> [Point a]
solveTspNearestInsertion [] = []
solveTspNearestInsertion [x] = [x]
solveTspNearestInsertion (x:[y]) = [x,y]
solveTspNearestInsertion (x:xs) = nearestInsertion edgeStartCycle (delete secondPoint xs)
    where startCycle = [x, secondPoint]
          edgeStartCycle = zip startCycle ((tail startCycle) ++ [head startCycle])
          secondPoint = nearestPoint x xs


files = ["pr107.tsp", "pr152.tsp", "pr439.tsp", "d198.tsp", "d493.tsp", "d657.tsp", "d2103.tsp"]


readTspLine :: String -> Maybe (Point Double)
readTspLine line
    | isDigit $ head line = Just $ (read first, read second) 
    | otherwise = Nothing
    where first = head $ tail $ words line
          second = last $ tail $ words line


readTspText :: String -> [Point Double]
readTspText text = catMaybes $ map readTspLine (lines text)


readTspFile :: String -> IO [Point Double]
readTspFile file = do
        text <- readFile file
        return $ readTspText text


getWorkTime :: (Floating a, Ord a) => ([Point Double] -> [Point Double]) -> [Point Double] -> IO (Integer, Double)
getWorkTime getTour points = do
        startTime <- getCPUTime
        let tourLength = calculateTourLength $ getTour points
        endTime <- getCPUTime
        return (endTime - startTime, tourLength)


runTspFile :: String -> IO ()
runTspFile file = do
        points <- readTspFile file
        (nNTime, nNLength) <- getWorkTime solveTspNearestNeighbour points
        (nITime, nILength) <- getWorkTime solveTspNearestInsertion points
        putStrLn (file ++ " done in " ++ (show nNTime) ++ " clocks with tour length " ++ (show nNLength) ++ " using NN and in "
                 ++ (show nITime) ++ " clocks with tour length " ++ (show nILength) ++ " using NI")

runAll :: [String] -> IO ()
runAll = sequence_ . map runTspFile 

main = do
        runAll files
