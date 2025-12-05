import Helpers
import qualified Data.Vector as Vec

type Grid = Vec.Vector (Vec.Vector Char)

main = do
    runner "inputs/day4_real.txt" solve1 solve2

solve1 :: [String] -> Int
solve1 lines =
    let vecLines = convertToVector lines
    in countRolls vecLines

countRolls :: Grid -> Int
countRolls grid =
    doTraverse grid 0 0 0

doTraverse :: Grid -> Int -> Int -> Int -> Int
doTraverse grid x y rolls =
    let xAtEnd = x == (Vec.length (grid Vec.! 0) - 1)
        yAtEnd = y == Vec.length grid - 1
        nextX = (if xAtEnd then 0 else x + 1)
        nextY = (if xAtEnd then y + 1 else y)
    in (if xAtEnd && yAtEnd then rolls else doTraverse grid nextX nextY (rolls + rollValue grid x y))

rollValue :: Grid -> Int -> Int -> Int
rollValue grid x y =
    case (grid Vec.! y) Vec.! x of '@' -> validateNeighbors grid (possibleNeighborPositions x y)
                                   _ -> 0

validateNeighbors :: Grid -> [(Int, Int)] -> Int
validateNeighbors grid neighborPairs =
    let neighborVals = map (\pair -> validateLocation grid pair) neighborPairs
        neighborValsWithRolls = filter (\v -> if v == Just '@' then True else False) neighborVals
        amount = length neighborValsWithRolls
    in 
        if amount < 4 then 1 else 0

validateLocation :: Grid -> (Int, Int) -> Maybe Char
validateLocation grid (x, y) =
    let row = grid Vec.!? y
    in case row of Just vec -> vec Vec.!? x
                   _ -> Nothing


possibleNeighborPositions :: Int -> Int -> [(Int, Int)]
possibleNeighborPositions x y =
    [(x + 1, y), (x + 1, y + 1), (x + 1, y - 1), (x, y - 1), (x, y + 1), (x - 1, y), (x - 1, y - 1), (x - 1, y + 1)]

solve2 :: [String] -> Int
solve2 lines =
    let vecLines = convertToVector lines
    in countRollsV2 vecLines

countRollsV2 :: Grid -> Int
countRollsV2 grid =
    doTraverseUntilAllRollsInaccessible grid 0 0 0 [] 0

doTraverseUntilAllRollsInaccessible :: Grid -> Int -> Int -> Int -> [(Int,Int)] -> Int -> Int
doTraverseUntilAllRollsInaccessible grid x y rolls removedPositions totalRolls =
    let xAtEnd = x == (Vec.length (grid Vec.! 0) - 1)
        yAtEnd = y == Vec.length grid - 1
        nextX = (if xAtEnd then 0 else x + 1)
        nextY = (if xAtEnd then y + 1 else y)
        valueOfCurrentPos = rollValueV2 grid removedPositions x y
        newRemovedPositions = if valueOfCurrentPos == 0 then removedPositions else (x,y):removedPositions
    in
        case (xAtEnd && yAtEnd) of True -> case rolls of 0 -> totalRolls
                                                         _ -> doTraverseUntilAllRollsInaccessible grid 0 0 0 newRemovedPositions (totalRolls + rolls) 
                                   False -> doTraverseUntilAllRollsInaccessible grid nextX nextY (rolls + valueOfCurrentPos) newRemovedPositions 0

rollValueV2 :: Grid -> [(Int,Int)] -> Int -> Int -> Int
rollValueV2 grid removedPositions x y =
    case (grid Vec.! y) Vec.! x of '@' -> if (x,y) `elem` removedPositions then 0 else validateNeighborsV2 grid (possibleNeighborPositions x y) removedPositions
                                   _ -> 0

validateNeighborsV2 :: Grid -> [(Int, Int)] -> [(Int, Int)] -> Int
validateNeighborsV2 grid neighborPairs removedPositions =
    let neighborVals = map (\pair -> validateLocationV2 grid pair removedPositions) neighborPairs
        neighborValsWithRolls = filter (\v -> if v == Just '@' then True else False) neighborVals
        amount = length neighborValsWithRolls
    in 
        if amount < 4 then 1 else 0

validateLocationV2 :: Grid -> (Int, Int) -> [(Int, Int)] -> Maybe Char
validateLocationV2 grid (x, y) removedPositions =
    let row = grid Vec.!? y
    in case row of Just vec -> if (not ((x,y) `elem` removedPositions)) then vec Vec.!? x else Just '.'
                   _ -> Nothing

convertToVector :: [String] -> Grid
convertToVector lines =
    Vec.fromList (map Vec.fromList lines)