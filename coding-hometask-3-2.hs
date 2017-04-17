import Data.List (minimumBy, delete)
import Data.Ord (comparing)
import Data.Maybe (fromJust, catMaybes)
import Data.Char (isDigit)
import Control.DeepSeq (deepseq)
import System.CPUTime (getCPUTime)
import qualified Data.Map.Strict as M


type Point a = (a, a)
type Vertex a = (a, a)
type Edge a = (Vertex a, Vertex a)
type Weighter a = Edge a -> a
type Tree a = [Edge a]


euclidianDistance :: (Floating a) => Point a -> Point a -> a
euclidianDistance (x1, y1) (x2, y2) = sqrt $ (x2 - x1)^2 + (y2 - y1)^2


euclidianWeighter :: (Floating a) => Edge a -> a
euclidianWeighter (a, b) = euclidianDistance a b


mst :: (Ord a) => [Vertex a] -> Weighter a -> Tree a
mst [] _ = []
mst [v] _ = []
mst (v:vs) weighter = prim startNearest []
    where startNearest = zip vs (replicate (length vs) v)
          prim [] tree = tree
          prim vs tree = prim newVs (newEdge:tree)
              where newEdge = minimumBy (comparing weighter) vs 
                    newVs = map recalc (delete newEdge vs)
                    recalc e
                        | weighter e <= weighter (fst e, fst newEdge) = e
                        | otherwise                                   = (fst e, fst newEdge)


getTreeWeight :: (Num a) => Tree a -> Weighter a -> a
getTreeWeight tree weighter = sum $ map weighter tree


makeWeighter :: (Floating a, Ord a) => M.Map (Point a) a -> Weighter a
makeWeighter map (u, v) = euclidianDistance u v - (map M.! u) - (map M.! v)


calculateDegree :: (Ord a) => Tree a -> [Vertex a] -> M.Map (Vertex a) Int
calculateDegree tree vertices = foldl addEdge (M.fromList (zip vertices (replicate (length vertices) 0))) tree
    where addEdge map edge = (M.update (Just . (+1)) (fst edge)) $ (M.update (Just . (+1)) (snd edge)) $ map


heldKarp :: (Ord a, Num a, Floating a) => (a, a, Int, M.Map (Vertex a) a) -> (a, a, Int, M.Map (Vertex a) a) 
heldKarp (oldLowerBound, const, iteration, vertices) = ((max oldLowerBound lowerBound), const / 2, iteration + 1, newVs)
    where tree = mst (M.keys vertices) (makeWeighter vertices)
          lowerBound = getTreeWeight tree euclidianWeighter
          newVs = M.mapWithKey recalc vertices
          degrees = calculateDegree tree (M.keys vertices)
          recalc k v = v + const * (2 - fromIntegral (degrees M.! k))
          

lowerBoundTsp :: (Floating a, Ord a) => [Point a] -> a
lowerBoundTsp points = result
    where (result, _, _, _) = until predicate heldKarp (0, 2, 0, M.fromList (zip points (replicate (length points) 0)))
          predicate (_, _, iteration, _) = iteration >= 5


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


getWorkTime :: (Integral a) => [Point Double] -> IO (Double, a)
getWorkTime points = do
        startTime <- getCPUTime
        let lowerBound = lowerBoundTsp points
        endTime <- lowerBound `deepseq` getCPUTime
        return (fromInteger (endTime - startTime) / 1e12 , round lowerBound)


runTspFile :: String -> IO ()
runTspFile file = do
        points <- readTspFile file 
        (time, lowerBound) <- getWorkTime points
        putStrLn ("Instance " ++ file ++ "... done in " ++ (show time) ++ " seconds with lower bound " ++ (show lowerBound))

runAll :: [String] -> IO ()
runAll = sequence_ . map runTspFile 

main = do
        runAll files
