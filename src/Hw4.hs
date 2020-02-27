module Hw4
    (
    Vec(..),
    range,
    absolute,
    rd,
    dft,
    fft) where

import Data.Complex
import GHC.Base

range :: Double -> Double -> Double -> [Double]
absolute :: Vec (Complex Double) -> Vec Double
rd :: Int -> Vec Double -> Vec Double
dft :: [Double] -> Vec (Complex Double)
fft :: [Double] -> Vec (Complex Double)

newtype Vec a = Vec {runVec :: [a]}

instance Show a => Show (Vec a) where
    show (Vec a) = "[" ++ show' a ++ "]"

show' :: Show a => [a] -> [Char]
show' [] = ""
show' [x] = show x
show' (x:xs) = show x ++ " " ++ show' xs

instance Functor Vec where
    fmap g x = Vec $ g <$> runVec x

instance Applicative Vec where
    pure x = Vec $ repeat x
    liftA2 f a b =  Vec $ zipWith f (runVec a) (runVec b)

instance Semigroup (Vec a) where
    (<>) (Vec a) (Vec b) = Vec $ a ++ b

instance Monoid (Vec a) where
    mempty = Vec []

instance Num a => Num (Vec a) where
    fromInteger x = Vec $ pure $ fromInteger x
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    negate (Vec a) = Vec $ fmap negate a
    abs (Vec a) = Vec $ fmap abs a
    signum (Vec a) = Vec $ fmap signum a

instance Fractional a => Fractional (Vec a) where
    (/) = liftA2 (/)
    fromRational x = Vec $ pure $ fromRational x

instance Floating a => Floating (Vec a) where
    exp (Vec a) = Vec (fmap exp a)
    log (Vec a) = Vec (fmap log a)
    sin (Vec a) = Vec (fmap sin a)
    cos (Vec a) = Vec (fmap cos a)
    asin (Vec a) = Vec (fmap asin a)
    acos (Vec a) = Vec (fmap acos a)
    atan (Vec a) = Vec (fmap atan a)
    sinh (Vec a) = Vec (fmap sinh a)
    cosh (Vec a) = Vec (fmap cosh a)
    asinh (Vec a) = Vec (fmap asinh a)
    acosh (Vec a) = Vec (fmap acosh a)
    atanh (Vec a) = Vec (fmap atanh a)
    pi = Vec $ pure pi

range from to count = fmap (\x -> from + x * step) [0..count-1] 
    where step = (to - from)/count

absolute = fmap (\x -> sqrt(realPart x * realPart x + imagPart x * imagPart x))

rd n = fmap (\x -> fromIntegral (round $ c * x) / c)
  where c = 10^n

dft x =
    let n = fromIntegral $ length x
        index = range 0 n n
        xn = x `zip` index

        f k = sum y 
                where 
                    y = map factor xn
                    factor (xi, j) = let y = 2 * pi * j * k / n             
                                        in xi * cos y :+ (-xi * sin y)
    in
        Vec $ fmap f index


fft x  
    | n <= 16 = dft x
    | otherwise = 
        let (even, odd) = split x 
            (e, o) = (fft even, fft odd)
            t = -2 * pi / fromIntegral n

            g (e', o', k) =  
                        let 
                            y = t * fromIntegral k 
                            (fr, fi) = (cos y, sin y)
                            (pr, pi) = (fr * realPart o' - fi * imagPart o', fr * imagPart o' + fi * realPart o')
                        in
                            ((realPart e' + pr) :+ (imagPart e' + pi), (realPart e' - pr) :+ (imagPart e' - pi))

            (lower, upper) = unzip $ fmap g $ zip3 (runVec e) (runVec o) [0..]
        in
            Vec (lower ++ upper)
        
    where 
            n = length x
            split [] = ([], [])
            split [a] = ([a], [])
            split (a:b:c) = (a:x, b:y)
                where (x,y) = split c