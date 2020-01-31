module Main where

import Hw1
-- import Numeric.LinearAlgebra

main :: IO ()
-- main = print(dft(rd 2 (range 0 1 10)))
-- main = print "lala"

-- linspace 5 (-3,7)
-- main = someFunc

-- main :: Integer -> Integer

-- main = print (lstsumIf [2,3,4,5])
-- main = squareFunc 10
main = do
    let n = 64
    let s = map (\t -> sin(10*2*pi*t) + sin(20*2*pi*t)/2) $ range 0 1 n
    let result = map (\x -> x/n) $ absolute $ dft s
    print s
    print(rd 3 s)
    print(rd 2 result)
