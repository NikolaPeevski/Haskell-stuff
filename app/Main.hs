module Main where

import Hw1
import Hw2
import Hw3
import Data.Time.Clock
-- import Numeric.LinearAlgebra

main :: IO ()
-- main = print(dft(rd 2 (range 0 1 10)))
-- main = print "lala"

-- linspace 5 (-3,7)
-- main = someFunc

-- main :: Integer -> Integer

-- main = print (lstsumIf [2,3,4,5])
-- main = squareFunc 10

f x = x + 1
main = do
    -- let n = 2^5
    -- let s1 = map (\x -> sin(20*pi*x) + sin(40*pi*x)/2) $ range 0 1 n 
    -- -- print $ split [1..64]
    -- -- print(rd 3 s1)
    -- start <- getCurrentTime
    -- let dft1 = map (\x -> x/n) $ absolute $ dft s1 
    -- print(rd 2 dft1)
    -- end <- getCurrentTime
    -- print (diffUTCTime end start)
    -- start2 <- getCurrentTime
    -- let fft1 = map (\x -> x/n) $ absolute $ fft s1 
    -- print(rd 2 fft1)
    -- end2 <- getCurrentTime
    -- print (diffUTCTime end2 start2)
    -- let v1 = Vec [1.0,2.0,3.0]
    -- let v2 = Vec [2,3,4]
    -- let v3 = Vec [-10,0,10]

    -- print $ v1 == v2
    -- print $ show v3
    -- print $ v1 + v2
    -- print $ v1 - v2
    -- print $ v1 * v2
    -- print $ v1 / v3
    -- print $ negate v1
    -- print $ signum v3
    -- print $ abs v3
    -- print $ foldr (*) 1 v1
    -- print $ sin $ v1 * (pi / 2)
    -- print $ v1 + (pure' $ sqrt 2)
    -- print $ realV v1
    -- print $ pure' 'a'

    let v1 = Vec [1,2,3]
    let v2 = Vec [2,3,4]
    let v3 = Vec [-10,0,10]
    print $ v1 + v2
    print $ v1 - v2
    print $ v1 * v2
    print $ v1 / v2
    print $ negate v1
    print $ signum v3
    print $ abs v3
    print $ v1 + 10
    print $ v2 + 1.2
    print $ v1 + (pure' $ sqrt 2)
    print $ realV v1
    print $ imagV v1
    print $ realV v1 + imagV v2
    print $ sin $ v1 * (pi / 2)
    print $ sum v1