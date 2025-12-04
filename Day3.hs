import Helpers
import Data.Char
import Data.List

main = do
    runner "inputs/day3_real.txt" solve1 solve2

solve1 :: [String] -> Int
solve1 lines =
    let intArrs = map getArrOfInts lines
    in sum $ map buildMaxJoltage intArrs

buildMaxJoltage :: [Int] -> Int
buildMaxJoltage ints =
    let possibleLefts = take (length ints - 1) ints
        leftDigit = maximum possibleLefts
        indexOfMax = elemIndex leftDigit ints
        indexAsInt = case indexOfMax of Just x -> x
                                        Nothing -> -1 -- not possible in a non-empty list
        rightDigit = maximum (drop (indexAsInt+1) ints)
        digits = show leftDigit ++ show rightDigit
    in
        read digits :: Int


getArrOfInts :: String -> [Int]
getArrOfInts = map digitToInt 


solve2 :: [String] -> Int
solve2 _ = 0
