{- stack script
    --resolver lts-16.15
    --ghc-options -Wall
    --package unordered-containers
-}

import Data.HashSet

-- Find the two entries that sum to 2020; multiply them together and return the value.

main :: IO ()
main = do
    content <- readFile "input.txt"
    let numbers = (read <$> words content) :: [Int]
    print (f empty numbers)

f :: HashSet Int -> [Int] -> Maybe Int
f s (x:xs) = case x `member` s of
    True -> Just ((2020 - x) * x)
    False -> f (insert (2020 - x) s) xs
f _ [] = Nothing
