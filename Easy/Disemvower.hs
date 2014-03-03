-- http://www.reddit.com/r/dailyprogrammer/comments/1ystvb/022414_challenge_149_easy_disemvoweler/

import Data.Char(isSpace)

main = do
    putStrLn "Insert line"
    sentence <- getLine
    let (noVowels, vowels) = disemvowel sentence
    putStrLn noVowels
    putStrLn vowels

vowels = ['a', 'e', 'i', 'o', 'u']

isVowel = \c -> elem c vowels

disemvowel :: String -> (String, String)
disemvowel = foldl split ([], [])
    where split (nvs, vs) h | isVowel h = (nvs, vs ++ [h])
                            | isSpace h = (nvs, vs)
                            | otherwise = (nvs ++ [h], vs)
