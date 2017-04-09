import Data.Set (member, Set, fromList, toList, difference, delete, insert)
import Data.List (minimumBy)
import Control.DeepSeq (deepseq)
import System.CPUTime (getCPUTime)
import Data.Ord (comparing)

type Edge a = (a, a)
type Graph a = ([a], [Edge a])


getQuality :: (Ord a) => Graph a -> Set a -> Int
getQuality (_, edges) part = foldl (\acc edge -> acc + checkEdge edge) 0 edges
    where checkEdge (v1, v2)
              | v1 `member` part && not (v2 `member` part) = 1
              | not (v1 `member` part) && v2 `member` part = 1
              | otherwise                                  = 0


getNeighbourhood :: (Ord a) => Graph a -> Set a -> [Set a]
getNeighbourhood (vertices, _) part1 = [insert v2 $ delete v1 part1 | v1 <- toList part1, v2 <- toList part2]
    where part2 = difference (fromList vertices)  part1


solveVariableDepthLocalSearch :: (Ord a) => Graph a -> Set a -> Int -> Set a
solveVariableDepthLocalSearch (vertices, edges) part depth = kernighanLin part part depth
    where kernighanLin startPart currentPart depth
              | bestWeight < startWeight    = kernighanLin bestNeighbour bestNeighbour depth
              | depth > 0                   = kernighanLin startPart bestNeighbour (depth - 1)
              | otherwise                   = startPart
              where startWeight = getQuality (vertices, edges) startPart
                    neighbourhood = getNeighbourhood (vertices, edges) currentPart
                    bestNeighbour = minimumBy (comparing (getQuality (vertices, edges))) neighbourhood 
                    bestWeight = getQuality (vertices, edges) bestNeighbour

variableDepthLocalSearch :: (Ord a) => Graph a -> Set a
variableDepthLocalSearch (vertices, edges) = solveVariableDepthLocalSearch (vertices, edges) (fromList startPart) depth
    where depth = 0
          startPart = take ((length vertices) `div` 2) vertices


files = ["add20.graph", "cti.graph", "t60k.graph", "m14b.graph"]


readFirstGraphLine :: String -> Int
readFirstGraphLine = read . head . words


readOtherGraphLines :: [String] -> Int -> [(Int, Int)]
readOtherGraphLines text verticesNumber = toList edges 
    where verEdges n = zip (replicate (length line) n) (map read (words line))
              where line = text !! (n - 1)
          orderedVerEdges n  = map (\(x, y) -> if x > y then (y,x) else (x,y)) (verEdges n)
          edges = fromList $ concat $ map orderedVerEdges [1,2..verticesNumber]


readGraphText :: String -> (Graph Int)
readGraphText text = ([1,2..verticesNumber], edges) 
    where textLines = lines text
          verticesNumber = readFirstGraphLine (head textLines)
          edges = readOtherGraphLines (tail textLines) verticesNumber 


readGraphFile :: String -> IO (Graph Int)
readGraphFile file = do
        text <- readFile file
        return $ readGraphText text


getWorkTime :: (Ord a) => Graph a -> IO (Double, Int)
getWorkTime graph = do 
        startTime <- getCPUTime
        let weight = getQuality graph (variableDepthLocalSearch graph)
        endTime <- weight `deepseq` getCPUTime
        return (fromInteger (endTime - startTime) / 1e12, weight)
        

runGraphFile :: String -> IO ()
runGraphFile file = do
        graph <- readGraphFile file
        (time, weight) <- getWorkTime graph
        putStrLn ("Solving instance" ++ file ++ "… done in" ++ (show time) ++ "seconds with quality" ++ (show weight))


runAllFiles :: [String] -> IO ()
runAllFiles = sequence_ . map runGraphFile


main = do
        runAllFiles files
