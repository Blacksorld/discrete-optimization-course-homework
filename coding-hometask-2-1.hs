import Data.Set (Set, difference, member, insert, delete, fromList, toList, empty)
import System.Environment
import System.IO

type Graph a = ([a], [(a, a)])

calculateEdgeNumber :: (Ord a) => Graph a -> Set a -> Int
calculateEdgeNumber (vertices, edges) part = foldl (\acc edge -> acc + checkEdge edge) 0 edges
    where checkEdge (v1, v2)
              | member v1 part && not (member v2 part) = 1
              | not (member v1 part) && member v2 part = 1
              | otherwise                              = 0

getNeighbourhood :: (Ord a) => Graph a -> Set a -> [Set a]
getNeighbourhood (vertices, edges) part1 = [insert v2 $ delete v1 part1 | v1 <- toList part1, v2 <- toList part2]
    where part2 = difference (fromList vertices)  part1

argMin :: (Ord b) => (a -> b) -> [a] -> Maybe a
argMin f [] = Nothing
argMin f (head:xs) = Just (foldl (\x acc -> check acc x) head xs)
    where check x y
              | f x <= f y = x
              | otherwise  = y

basicLocalSearch :: (Ord a) => Graph a -> (Set a, Set a, Int)
basicLocalSearch ([], _) = (empty, empty, 0)
basicLocalSearch (vertices, edges) = findBetterPartition (Just (fromList (take ((length vertices) `div` 2) vertices)))
    where findBetterPartition Nothing = (empty, empty, 0)
          findBetterPartition (Just part1)
              | (neighbourEdgeNumber newBestPart) < currentEdgeNumber = findBetterPartition newBestPart 
              | otherwise                                             = (part1, part2, currentEdgeNumber)
              where neighbourhood       = getNeighbourhood (vertices, edges) part1 
                    currentEdgeNumber   = calculateEdgeNumber (vertices, edges) part1
                    newBestPart         = argMin (calculateEdgeNumber (vertices, edges)) neighbourhood
                    part2               = difference (fromList vertices)  part1
                    neighbourEdgeNumber Nothing            = 0
                    neighbourEdgeNumber (Just newBestPart) = calculateEdgeNumber (vertices, edges) newBestPart 
                     

buildVertices :: [String] -> [Int]
buildVertices (x:xs)
    | x !! 0 == 'p' = [1,2..read ((words x) !! 2)]
    | otherwise     = buildVertices xs

tuplify2 :: [a] -> (a,a)
tuplify2 [x,y] = (x,y)

buildEdges :: [String] -> [(Int, Int)]
buildEdges content = (foldl (\acc xs -> tryGetEdge acc xs) [] content)
    where tryGetEdge acc (x:xs)
              | x == 'e' = ((tuplify2 (map read (words xs))):acc)
              | otherwise = acc

main = do
    args <- getArgs
    handle <- openFile (args !! 0) ReadMode
    contents <- hGetContents handle
    print (basicLocalSearch ((buildVertices (lines contents)), (buildEdges (lines contents))))
    hClose handle
