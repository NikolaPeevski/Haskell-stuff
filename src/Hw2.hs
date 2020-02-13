
module Hw2 (
    split,
    fft
    )
    where

import Hw1

split :: [Double] -> ([Double], [Double])
computeUV :: Double -> Double -> (Double, Double)
computeYk :: (Double, Double) -> (Double, Double) -> Double -> Double -> ((Double, Double), (Double, Double))
innerFFT :: Double -> Double -> [(Double, Double)] -> [(Double, Double)] -> [(Double, Double)]
fft :: [Double] -> [(Double, Double)]

split = foldr (\x y -> (x:snd y,fst y)) ([],[])

computeUV k n = (cos (-((2 * pi) / n) * k), sin (-((2 * pi) / n) * k))

computeYk (a, b) (c, d) k n = 
    let (u, v) = computeUV k n
        in
            ((a + u * c - v * d, b + u * d + v * c), (a - u * c + v * d, b - u * d - v * c) )

innerFFT 0 n (e1:e2) (o1:o2) =
    [x, y] where (x, y) =  computeYk e1 o1 0 n
innerFFT k n (e1: e2) (o1: o2) =
    let (x,y) = computeYk e1 o1 k n in
        xy : x : yx ++ [y] where (xy: yx) = innerFFT (k - 1) n e2 o2

fft x
    | length x <= 16 = dft x
    | otherwise =
        let (x1, x2) = split x
            e = fft x2
            o = fft x1
            n = fromIntegral (length x) in
                innerFFT ((n / 2)-1) n e o