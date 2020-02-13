module Hw1
    ( 
        range,
        rd,
        absolute,
        dft
    ) where

range :: Double -> Double -> Double -> [Double]
rd :: Int -> [Double] -> [Double]
absolute :: [(Double, Double)] -> [Double]
dft :: [Double] -> [(Double, Double)]
sinSum :: [Double] -> Double -> Double -> Double -> Double
cosSum :: [Double] -> Double -> Double -> Double -> Double

-- Range implementation using tail recursion
range from to count =
    innerRange from to count (count-1)
    where
        innerRange a _ _ 0 = [a]
        innerRange a b c d =
            let step = (b - a) / c
            in
                 innerRange a b c (d-1) ++ [a + d * step]

-- Round implementation using pattern matching
-- rd x [] = []
-- rd x (a:b) = fromIntegral (round (a * z)) / z : rd x b
--     where z = 10^x

rd _ [] = []
rd n (a:b) = f a : rd n b
  where f x = fromIntegral (round (c * x)) / c
        c = 10 ^ n

-- Absolute implementation using map
absolute = map (\ a -> sqrt (fst a * fst a + snd a * snd a))

-- Helper sin function
sinSum [] _ _ _ = 0
sinSum (a:b) k n 0 = a * sin((2 * pi * k * 0)/ n)
sinSum (a:b) k n n' = a * sin((2 * pi * k * n')/ n) + sinSum b k n (n' - 1)  

-- Helper cos function
cosSum [] _ _ _ = 0
cosSum (a:b) k n 0 = a * cos((2 * pi * k * 0)/ n)
cosSum (a:b) k n n' = a * cos((2 * pi * k * n')/ n) + cosSum b k n (n' - 1)  
    

-- Discrete Fourier Transform implementation using helper functions and tail recursion
dft x =
    innerDft x (fromIntegral (length x)) (fromIntegral (length x))
    where
        innerDft _ 0 _ = []
        innerDft x i l =
            (sinSum x i l l, cosSum x i l l) : innerDft x (i-1) l
    
