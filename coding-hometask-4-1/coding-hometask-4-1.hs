import Data.Array (array, (!), Array)
import Numeric.Limp.Program
import Numeric.Limp.Rep.IntDouble
import Numeric.Limp.Solvers.Cbc (solve)
import Numeric.Limp.Rep.Rep
import Data.List (intercalate, delete)


distanceMatrix1 :: Array (Int, Int) Int
distanceMatrix1 = array ((1, 1), (15, 15)) (zip [(x,y) | x <- [1..15], y <- [1..15]] distanceList)
    where distanceList = [0, 29, 82, 46, 68, 52, 72, 42, 51, 55, 29, 74, 23, 72, 46, 
                          29, 0, 55, 46, 42, 43, 43, 23, 23, 31, 41, 51, 11, 52, 21, 
                          82, 55, 0, 68, 46, 55, 23, 43, 41, 29, 79, 21, 64, 31, 51, 
                          46, 46, 68, 0, 82, 15, 72, 31, 62, 42, 21, 51, 51, 43, 64, 
                          68, 42, 46, 82, 0, 74, 23, 52, 21, 46, 82, 58, 46, 65, 23, 
                          52, 43, 55, 15, 74, 0, 61, 23, 55, 31, 33, 37, 51, 29, 59, 
                          72, 43, 23, 72, 23, 61, 0, 42, 23, 31, 77, 37, 51, 46, 33, 
                          42, 23, 43, 31, 52, 23, 42, 0, 33, 15, 37, 33, 33, 31, 37, 
                          51, 23, 41, 62, 21, 55, 23, 33, 0, 29, 62, 46, 29, 51, 11, 
                          55, 31, 29, 42, 46, 31, 31, 15, 29, 0, 51, 21, 41, 23, 37, 
                          29, 41, 79, 21, 82, 33, 77, 37, 62, 51, 0, 65, 42, 59, 61, 
                          74, 51, 21, 51, 58, 37, 37, 33, 46, 21, 65, 0, 61, 11, 55, 
                          23, 11, 64, 51, 46, 51, 51, 33, 29, 41, 42, 61, 0, 62, 23, 
                          72, 52, 31, 43, 65, 29, 46, 31, 51, 23, 59, 11, 62, 0, 59, 
                          46, 21, 51, 64, 23, 59, 33, 37, 11, 37, 61, 55, 23, 59, 0]


tspProgram :: Int -> Array (Int, Int) Int -> Program (Int, Int) Int IntDouble
tspProgram n costs = minimise target constraints bounds
    where vertices = [1..n]
          edges = [(x, y) | x <- vertices, y <- vertices, x /= y]
          target = LZ (zip edges (map (Z . (!) costs) edges)) 0
          bounds = map binary edges
          degreeConstraints = [constr v vEdges1 | v <- vertices] ++ [constr v vEdges2 | v <- vertices]
              where filterFun (x, y) = x /= y
                    vEdges1 v = filter filterFun (zip (replicate n v) vertices)
                    vEdges2 v = filter filterFun (zip vertices (replicate n v))
                    constr v edges = (LZ (zip (edges v) (replicate (length (edges v)) (Z 1))) 0) :== c1
          mtzConstraints = [r1 ui .-. r1 uj  .+. z (ui, uj) (Z n) :<= con (Z (n - 1)) | ui <- [2..n], uj <- [2..n], ui /= uj]
          constraints = foldl1 (:&&) (degreeConstraints ++ mtzConstraints)



unwrapZ :: Z IntDouble -> Int
unwrapZ (Z d) = d


getWay :: [(Int, Int)] -> [Int]
getWay edges = getWay' edges [] 1
    where getWay' [] way _ = reverse way
          getWay' edges way next = getWay' newEdges newWay newNext 
              where [edge] = filter (\(x, y) -> x == next) edges
                    newEdges = delete edge edges
                    newWay = (next:way)
                    newNext = snd edge


solveTspWithLp :: Int -> Array (Int, Int) Int -> (Int, [Int])
solveTspWithLp n costs = (cost, way)
    where solution = case solve $ tspProgram n costs of Left _ -> error "can't solve"
                                                        Right x -> x
          edges = filter ((==) 1 . unwrapZ . zOf solution) [(x, y) | x <- [1..n], y <- [1..n], x /= y]
          cost = sum $ map ((!) costs) edges
          way = getWay edges


showTspSolution :: (Int, [Int]) -> String
showTspSolution (cost, way) = "Cost of tour: " ++ show cost ++ "\n" ++ intercalate "→" (map show way)


main = do
        putStrLn $ showTspSolution $ solveTspWithLp 15 distanceMatrix1
