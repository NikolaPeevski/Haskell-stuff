
module Hw3
    (Vec(..),
    pure',
    realV,
    imagV ) where

import Data.Complex
        
-- {-# LANGUAGE FlexibleInstances #-}
-- {-# LANGUAGE UndecidableInstances #-}
-- {-# LANGUAGE RebindableSyntax #-}
-- TODO:
-- Show
-- Num: +, -, *, negate, abs, sigsum, fromInteger
-- Fractional: /, fromRational
-- Floating: pi, exp, log, sin, cos, asin, acos, atan, sinh, cosh, asinh, acosh, atanh
-- Foldable: foldr
-- pure
-- realV
-- imagV 

data Vec a = Vec [a]

instance Show a => Show (Vec a) where
    show (Vec a) = "[" ++ show' a ++ "]"

show' :: Show a => [a] -> [Char]
show' [] = ""
show' [x] = show x
show' (x:xs) = show x ++ " " ++ show' xs

instance Num a => Num (Vec a) where
    fromInteger x = Vec $ repeat $ fromInteger x
    (+) (Vec a) (Vec b) = Vec (zipWith (+) a b)
    (-) (Vec a) (Vec b) = Vec (zipWith (-) a b)
    (*) (Vec a) (Vec b) = Vec (zipWith (*) a b)
    negate (Vec a) = Vec (map (\x -> x * (-1)) a)
    abs (Vec a) = Vec (map abs a)
    signum (Vec a) = Vec (map signum a)

instance Fractional a => Fractional (Vec a) where
    (/) (Vec a) (Vec b) = Vec (zipWith (/) a b)
    fromRational x = Vec $ repeat $ fromRational x

instance Floating a => Floating (Vec a) where
    exp (Vec a) = Vec (map exp a)
    log (Vec a) = Vec (map log a)
    sin (Vec a) = Vec (map sin a)
    cos (Vec a) = Vec (map cos a)
    asin (Vec a) = Vec (map asin a)
    acos (Vec a) = Vec (map acos a)
    atan (Vec a) = Vec (map atan a)
    sinh (Vec a) = Vec (map sinh a)
    cosh (Vec a) = Vec (map cosh a)
    asinh (Vec a) = Vec (map asinh a)
    acosh (Vec a) = Vec (map acosh a)
    atanh (Vec a) = Vec (map atanh a)
    pi = Vec $ repeat pi

instance Foldable Vec where
    foldr f x (Vec a) = foldr f x a
    
pure' x = Vec $ repeat x
realV :: Num a => Vec a -> Vec (Complex a)
imagV :: Num a => Vec a -> Vec (Complex a)
realV (Vec a) = Vec(map (:+ 0) a)
imagV (Vec a) = Vec(map (0:+) a)