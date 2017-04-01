import Data.List (minimumBy, delete)

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
solveTspNearestNeighbour (x:xs) = (nextPoint:(solveTspNearestNeighbour (delete nextPoint xs)))
    where nextPoint = nearestPoint x xs

solveTspNearestInsertion :: (Floating a, Ord a) => [Point a] -> [Point a]
solveTspNearestInsertion [] = []
solveTspNearestInsertion [x] = [x]
solveTspNearestInsertion (x:[y]) = [x,y]
solveTspNearestInsertion (x:xs) = nearestInsertion (x:secondPoint) (delete secondPoint xs)
    where secondPoint = nearestPoint x xs
          nearestInsertion cycle [] = cycle
          nearestInsertion cycle xs = nearestInsertion newCycle (delete nextPoint xs)
          nextPoint = snd $ minimumBy (comparator) [(x, y) | x <- cycle, y <- xs]
              where comparator a b = euclidianDistance (fst a snd a) `compare` euclidianDistance (fst b snd b)
           
