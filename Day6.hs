
import Helpers
import Data.Char

main = do
    runner "inputs/day6_real.txt" solve1 solve2

solve1 :: [String] -> Int
solve1 lines =
    let problems = map words lines
    in solveProblems problems
    

solveProblems :: [[String]] -> Int
solveProblems problems =
    case length (head problems) of 0 -> 0
                                   _ -> solveProblem (map head problems) + solveProblems (map tail problems)

solveProblem :: [String] -> Int
solveProblem problem =
    let nums = map (\n -> read n :: Int) (init problem)
        op = last problem
    in
        case op of "*" -> product nums
                   "+" -> sum nums

solve2 :: [String] -> Int
solve2 _ = 0

debug :: IO ()
debug = do
    contents <- readFile "inputs/day6_test.txt"
    print (lines contents)

-- ["123 328  51 64 "," 45 64  387 23 ","  6 98  215 314","*   +   *   +  "] --