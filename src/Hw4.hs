module Hw4
    ()
        where

import Data.Complex

newtype Vec = Vec {runVec :: [a]}

-- Functor functions: fmap
-- Applicative functions: pure liftA2
-- Semigroup <> operator
-- Monoid functions: mempty

-- change Num, Fractional, and Floating of hw3 to use
-- fmap, pure, and liftA2 

-- change dft and fft from hw2
-- range :: Double -> Double -> Double -> [Double]
-- absolute :: Vec (Complex Double) -> Vec Double
-- rd :: Int -> Vec Double -> Vec Double
-- dft :: [Double] -> Vec (Complex Double)
-- fft :: [Double] -> Vec (Complex Double)