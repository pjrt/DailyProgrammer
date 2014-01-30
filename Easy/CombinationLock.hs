{-http://www.reddit.com/r/dailyprogrammer/comments/1v4cjd/011314_challenge_148_easy_combination_lock/-}

import System.Environment(getArgs)

type Size = Int
type Code = Int
type Pos = Int
type Delta = Int
data Direction = Clockwise | Counter

rotate :: Direction -> Size -> Pos -> Pos -> Delta
rotate Clockwise n iPos fPos =
        let lock = cycle [0 .. n]
            delta = (length $ takeWhile (/= fPos) $ dropWhile (/= iPos) lock)
        in if delta == 0 then n else delta

rotate Counter n iPos fPos =
        let lock = cycle $ reverse [0 .. n]
            delta = length (takeWhile (/= fPos) $ dropWhile (/= iPos) lock) -1
        in if delta == -1 then n else delta

findRotations :: Size -> Code -> Code -> Code -> Int
findRotations n a b c =
        let  del1 = 2 * rotate Clockwise n 0 0
             del2 = rotate Clockwise n 0 a
             del3 = rotate Counter n a a
             del4 = rotate Counter n a b
             del5 = rotate Clockwise n b c
        in del1 + del2 + del3 + del4 + del5

main = do args <- getArgs
          case map read args of
              [n, a, b, c] -> print $ findRotations n a b c
              _ -> error "Expects an input of 4 integers"
