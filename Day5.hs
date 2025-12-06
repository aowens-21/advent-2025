import Helpers
import Data.List
import Data.List.Split
import qualified Data.Set as Set
import Data.Maybe

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
solve2 lines =
    let (ranges, _) = parseInput lines
        rangesWithNoOverlap = collapseRangesUntilSettle ranges
    in 
        sum $ map (\(s,e) -> (e - s) + 1) rangesWithNoOverlap

collapseRangesUntilSettle :: [(Int, Int)] -> [(Int, Int)]
collapseRangesUntilSettle ranges =
    let collapsed = collapseRanges ranges
    in
        if length collapsed == length ranges
        then collapsed
        else collapseRangesUntilSettle collapsed

collapseRanges :: [(Int,Int)] -> [(Int, Int)]
collapseRanges allRanges =
    nub $ collapseRanges' allRanges 0

collapseRanges' :: [(Int, Int)] -> Int -> [(Int, Int)]
collapseRanges' ranges idx =
    if idx >= length ranges then ranges else
    let range = ranges !! idx
        firstOverlap = find (\e -> e /= range && isJust (getOverlap range e)) ranges
    in
        case firstOverlap of Nothing -> collapseRanges' ranges (idx + 1)
                             Just range2 -> let rangesWithout = filter (\r -> r /= range && r /= range2) ranges
                                                overlap = getOverlap range range2
                                            in
                                                collapseRanges' (rangesWithout ++ [fromJust overlap]) (idx + 1)

getOverlap :: (Int, Int) -> (Int, Int) -> Maybe (Int, Int)
getOverlap (s1, e1) (s2, e2) =
    if s2 > e1 || s1 > e2
    then Nothing
    else Just (min s1 s2, max e1 e2)

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