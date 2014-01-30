-- http://www.reddit.com/r/dailyprogrammer/comments/1undyd/010714_challenge_147_easy_sport_points/

import System.Environment(getArgs)

getCombinations :: Int -> [(Int, Int, Int, Int)]
getCombinations intPoints =
        [ (w, x, y, z) |
        w <- [0..intPoints],
        x <- [0..intPoints],
        y <- [0..intPoints],
        z <- [0..intPoints],
        8*w + x*7 + y*6 + z*3 == intPoints
        ]

isValidPoints :: Int -> Bool
isValidPoints points =
        let combinations = getCombinations points
        in not $ null combinations

main = do
    (points:_) <- getArgs
    print (if isValidPoints $ read points then "Valid Score" else  "Invalid Score")
