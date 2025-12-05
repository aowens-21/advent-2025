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

getMaxInArrWithLeftover :: [Int] -> Int -> [Int] -> [Int]
getMaxInArrWithLeftover arr leftover acc =
    case leftover of 0 -> acc
                     _ -> let possibleDigits = case leftover of 1 -> arr
                                                                _ -> take (length arr - leftover + 1) arr 
                              leftDigit = maximum possibleDigits
                              indexOfMax = elemIndex leftDigit arr
                              indexAsInt = case indexOfMax of Just x -> x
                                                              Nothing -> -1 -- not possible in a non-empty list
                              rest = drop (indexAsInt + 1) arr
                          in
                            getMaxInArrWithLeftover rest (leftover - 1) (leftDigit:acc)

getArrOfInts :: String -> [Int]
getArrOfInts = map digitToInt 

solve2 :: [String] -> Int
solve2 lines = 
    let intArrs = map getArrOfInts lines
        reversedVoltagesAsArrays = map (\arr -> getMaxInArrWithLeftover arr 12 []) intArrs
        voltagesAsArrays = map reverse reversedVoltagesAsArrays
        voltagesAsStrings = map (concatMap show) voltagesAsArrays 
        voltagesAsNums = map (\v -> read v :: Int) voltagesAsStrings
    in sum voltagesAsNums