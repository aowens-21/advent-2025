import Helpers
import Data.List.Split
import Data.List

main = do
    runner "inputs/day2_real.txt" solve1 solve2

solve1 :: [String] -> Int
solve1 linesOfFile =
    let allIds = (generateRanges . parseLines) linesOfFile
    in sum $ filter isValid allIds

isValid :: Int -> Bool
isValid id =
    let idStr = show id
    in case length idStr `mod` 2 of   0 -> let [p1, p2] = chunksOf (length idStr `div` 2) idStr
                                           in p1 == p2
                                      _ -> False

solve2 :: [String] -> Int
solve2 linesOfFile =
    let allIds = (generateRanges . parseLines) linesOfFile
    in sum $ filter isValidV2 allIds

isValidV2 :: Int -> Bool
isValidV2 id =
    let idStr = show id
        possibleChunks = genPossibleChunks idStr
    in
        anyChunksValid possibleChunks 

anyChunksValid :: [[String]] -> Bool
anyChunksValid chunks =
    any (\chunkList -> (length (group chunkList) == 1)) chunks

genPossibleChunks :: String -> [[String]]
genPossibleChunks str =
    let half = length str `div` 2
        sizes = [1..half]
    in map (\ size -> chunksOf size str) sizes

parseLines :: [String] -> [String]
parseLines lines =
    filter (/= "") (concatMap (splitOn ",") lines)

generateRanges :: [String] -> [Int]
generateRanges rangeStrs =
    concatMap generateRange rangeStrs

generateRange :: String -> [Int]
generateRange rangeStr =
    let [startStr, endStr] = splitOn "-" rangeStr
        start = read startStr :: Int
        end = read endStr :: Int
    in [start..end]


