-- http://www.reddit.com/r/dailyprogrammer/comments/1t0r09/121613_challenge_145_easy_tree_generation/
import System.Environment(getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        (_n:_baseC:_leafC:_) -> do
            let (n, baseC, leafC) = (read _n, head _baseC, head _leafC)
            if even n || n < 3
            then error "n must be odd AND larger than 3"
            else do
                let tree = constructTree n baseC leafC
                mapM_ putStrLn $ reverse tree

        _                 -> error "No enough arguments given"

constructTree :: Int -> Char -> Char -> [[Char]]
constructTree n bC lC = padTree n $ replicate 3 bC : mkLine n lC
    where mkLine :: Int -> Char -> [[Char]]
          mkLine 1 l = [[l]]
          mkLine n' l = replicate n' l : mkLine (n' - 2) l

padTree :: Int -> [[Char]] -> [[Char]]
padTree _ [] = []
padTree maxSize tree = map stretch tree
    where stretch :: [Char] -> [Char]
          stretch line =
              let diff = maxSize - length line `div` 2
              in  replicate diff ' ' ++ line
