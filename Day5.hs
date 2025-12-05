import Helpers
import Data.List
import Data.List.Split
import qualified Data.Set as Set

main = do
    runner "inputs/day5_real.txt" solve1 solve2

solve1 :: [String] -> Int
solve1 lines =
    let (freshIdRanges, idsToCheck) = parseInput lines
    in length $ filter (isIdFresh freshIdRanges) idsToCheck

isIdFresh :: [(Int, Int)] -> Int -> Bool
isIdFresh [] id = False
isIdFresh ((start, end):rst) id =
    (id >= start && id <= end) || isIdFresh rst id


solve2 :: [String] -> Int
solve2 lines = -1

parseInput :: [String] -> ([(Int,Int)], [Int])
parseInput lines =
    let emptyIndex = elemIndex "" lines
        emptyIndex' = case emptyIndex of Just a -> a
                                         Nothing -> -1 -- not possible
        (rangeStrs, ingredientIdStrs) = splitAt emptyIndex' lines
        ranges = map buildRange rangeStrs
    in
        (ranges, map (\i -> read i :: Int) (tail ingredientIdStrs))

buildRange :: String -> (Int, Int)
buildRange rangeStr =
    let [start, end] = splitOn "-" rangeStr
        startNum = read start :: Int
        endNum = read end :: Int
    in (startNum, endNum)