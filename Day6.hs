
import Helpers
import Data.Char
import Data.List.Split
import Data.List (findIndex)

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
solve2 lines =
    let problems = processLines lines
    in sum (map processProblem problems)
 
processLines :: [String] -> [[String]]
processLines lines =
    let lineLen = length (head lines)
        range = [lineLen-1,lineLen-2..0]
    in  createProblems (filter (\s -> (not (all isSpace s))) (map (\i -> (map (\s -> s!!i) lines)) range))

createProblems :: [String] -> [[String]]
createProblems allItems =
    createProblems' allItems []
    where 
        createProblems' [] acc = acc
        createProblems' items acc =
            let endOfCurrProblemMaybe = findIndex (\numStr -> last numStr == '+' || last numStr == '*') items
                endOfCurrProblem = case endOfCurrProblemMaybe of Nothing -> error "Impossible!"
                                                                 Just i -> i
                currProblem = take (endOfCurrProblem + 1) items
                restOfProblems = drop (endOfCurrProblem + 1) items
            in createProblems' restOfProblems ([currProblem] ++ acc)

processProblem :: [String] -> Int
processProblem parts =
    let allButLast = init parts
        lastWithOp = last parts
        op = last lastWithOp
        lastWithoutOp = init lastWithOp
        allNumsAsStrs = allButLast ++ [lastWithoutOp]
        allNums = map (\s -> read s :: Int) allNumsAsStrs
    in
        case op of '+' -> sum allNums
                   '*' -> product allNums