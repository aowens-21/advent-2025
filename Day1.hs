import Helpers

main = do
    runner "inputs/day1_real.txt" solve1 solve2

solve1 :: [String] -> Int
solve1 lines =
    let nums = map parseLine lines
    in fst $ foldl folder (0, 50) nums

folder :: (Int, Int) -> Int -> (Int, Int)
folder (password, rotation) change =
    let newRot = (rotation + change) `mod` 100
    in case newRot of 0 -> (password + 1, newRot)
                      _ -> (password, newRot)

parseLine :: String -> Int
parseLine line =
    case head line of   'L' -> - (read num :: Int)
                        'R' ->  (read num :: Int)
    where num = tail line

solve2 :: [String] -> Int
solve2 lines =
    let nums = map parseLine lines
    in fst $ foldl folder2 (0, 50) nums

folder2 :: (Int, Int) -> Int -> (Int, Int)
folder2 (password, rotation) change =
    let newRotNoMod = (rotation + change)
        newRot = newRotNoMod `mod` 100
    in case newRotNoMod `compare` 0 of GT -> (password + (newRotNoMod `div` 100), newRot)
                                       LT -> case rotation of 0 -> (password + (abs newRotNoMod `div` 100), newRot)
                                                              _ -> (password + 1 + (abs newRotNoMod `div` 100), newRot)
                                       EQ -> (password + 1, 0)