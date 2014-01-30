-- http://www.reddit.com/r/dailyprogrammer/comments/1tiz4z/122313_challenge_140_intermediate_graph_radius/

import System.Environment
import Data.List
import Debug.Trace(traceShow)
import Data.Maybe(maybeToList)

traceS :: Show a => String -> a -> a
traceS s a = traceShow (s ++ show a) a

type Node = Int
type Graph = [(Node, [Node])]
type Matrix = [[Node]]

main = do
    (size:graphStr:_) <- getArgs
    let matrix = buildMatrix graphStr
        graph = buildGraph (read size) matrix
        allNodes = map fst graph
        radius = minimum $ map (findEcc graph allNodes) allNodes
    print radius

buildMatrix :: String -> Matrix
buildMatrix = map (map read . words) . lines

buildGraph :: Int -> Matrix -> Graph
buildGraph size = zip [0..] . map go
    where go line = map snd $ filter ((==1) . fst) $ line `zip` [0.. (size - 1)]

expandView :: Graph -> [Node] -> [Node]
expandView graph nodes = nub $ concatMap go nodes
    where go :: Node -> [Node]
          go node = concat $ maybeToList $ lookup node graph

findEcc :: Graph -> [Node] -> Node -> Int
findEcc graph allNodes node = findEcc' allNodes [] [node] 0
    where findEcc' notSeen seen currentView depth =
            let seen' = seen ++ currentView
                currentView' = expandView graph currentView
                notSeen' = notSeen \\ (seen' ++ currentView')
            in if null notSeen
                 then depth
                 else findEcc' notSeen' seen' currentView' $ depth + 1
