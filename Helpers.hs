module Helpers (
    runner
) where 

import System.IO ()

runner :: FilePath -> ([String] -> Int) -> ([String] -> Int) -> IO ()
runner fileName p1Solve p2Solve = do
    contents <- readFile fileName
    print (p1Solve $ lines contents)
    print (p2Solve $ lines contents)