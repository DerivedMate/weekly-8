module Lib where
import Data.List
import System.CPUTime
import System.Random

-- problem 1, kinda, because it accepts a list of ints, instead of just 3
trikuMiku :: [Int] -> Bool
trikuMiku ns = maximum ns == minimum ns

-- problem 1, strictly 3
threeEqs :: Int -> Int -> Int -> Bool
threeEqs a b c = trikuMiku [a,b,c]

-- problem 2
findTheGcd :: Int -> Int -> Int
findTheGcd = gcd

loadZeDice :: [Int] -> Int -> IO [Int]
loadZeDice ns throws = getStdGen 
    >>= return 
    . take throws 
    . map ((!!) ns) 
    . randomRs (0, length ns - 1)

-- problem 3
testDice :: [Int] -> IO [Int]
testDice dice = loadZeDice dice 100000
    >>= return 
    . map length 
    . group 
    . sort  