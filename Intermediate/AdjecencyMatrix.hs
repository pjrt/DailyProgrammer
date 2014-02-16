{-# LANGUAGE TupleSections #-}
-- http://www.reddit.com/r/dailyprogrammer/comments/1t6dlf/121813_challenge_140_intermediate_adjacency_matrix/

import Control.Applicative((<$>))
import Data.List.Split
import Data.Map.Lazy(fromListWith, toAscList)
import Control.Lens
import Data.Maybe(maybeToList)
import Debug.Trace(traceShow)

type Node = Int
type Edge = (Node, Node)
type NodeWithEdges = (Node, [Node])

main = do
    contents <- getContents
    let (nm, edgesStr) = splitAt 1 $ lines contents
        [n, m] =  words $ concat nm
        edges = convertEdges edgesStr
    putStr $ unlines $ map unwords $ showAdjGraph edges (read n) (read m)


convertEdge :: String -> [Edge]
convertEdge edgesStr =
        let [fromsStr, tosStr] = words <$> splitOn "->" edgesStr
            (froms, tos) = (read <$> fromsStr, read <$> tosStr)
        in concatMap (go tos) froms
    where go :: [Node] -> Node -> [Edge]
          go tos fr = map (fr,) tos

convertEdges :: [String] -> [Edge]
convertEdges edgesStrs = concatMap convertEdge edgesStrs

showAdjGraph :: [Edge] -> Int -> Int-> [[String]]
showAdjGraph edges sizeN sizeM =
        let nodesWithEdges = aggregateEdges edges
        in map (buildStr nodesWithEdges) [0..(sizeN - 1)]
    where buildStr :: [NodeWithEdges] -> Node -> [String]
          buildStr nodes node =
              let baseStr = map (const 0) [0..(sizeM -1)]
                  nodeEdges = concat $ maybeToList $ lookup node nodes
              in show <$> foldl (f) baseStr nodeEdges
              where f :: [Int] -> Node -> [Int]
                    f acc edg = acc & element edg .~ 1

aggregateEdges :: [Edge] -> [NodeWithEdges]
aggregateEdges edges = toAscList $ fromListWith (++) $ map (\t -> (fst t, [snd t])) edges
