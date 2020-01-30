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


range from to count =
    innerRange from to count (count-1)
    where
        innerRange a _ _ 0 = [a]
        innerRange a b c d =
            let step = (b - a) / c
            in
                 innerRange a b c (d-1) ++ [a + d * step]

rd x [] = []
rd x (a:b) = fromIntegral (floor (a * z)) / z : rd x b
    where z = 10^x

absolute = map (\ a -> sqrt (fst a * fst a + snd a * snd a))

sinSum (a:b) k n 0 = a * sin((2 * pi * k * 0)/ n)
sinSum (a:b) k n n' = a * sin((2 * pi * k * n')/ n) + sinSum b k n (n' - 1)  

cosSum (a:b) k n 0 = a * cos((2 * pi * k * 0)/ n)
cosSum (a:b) k n n' = a * cos((2 * pi * k * n')/ n) + cosSum b k n (n' - 1)  
    

dft x =
    innerDft :: [Double] -> Int -> Int -> [(Double, Double)]
    innerDft x (length x) - 1 (length x) - 1
    where
        innerDft [] _ _ = []
        innerDft (a:b) i l =
            [(sinSum a i l l, cosSum a i l l)] ++ innerDft b (i-1) l

    
