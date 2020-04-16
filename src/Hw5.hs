module Hw5 
    (Signal,
    Vec(..),
    rd,
    absolute,
    range,
    dft,
    idft,
    realV,
    low_pass',
    low_pass,
    fft,
    ifft,
    makeNoise)
    where

import Data.Complex
import Debug.Trace
import GHC.Base (liftA2)
import System.Random
-- import Control.Monad.Reader

-- {-# LANGUAGE  KindSignatures  #-}
-- {-# LANGUAGE  MultiParamTypeClasses  #-}

newtype Vec a = Vec {runVec :: [a]}
newtype  Reader r a = Reader { runReader  :: r -> a }
type Signal = Vec (Complex Double)

imagV :: Num a => Vec a -> Vec (Complex a)
realV :: Num a => Vec a -> Vec (Complex a)
range :: Double -> Double -> Double -> [Double]
absolute :: Vec (Complex Double) -> Vec Double
rd :: Int -> Vec Double -> Vec Double
twiddle :: Double -> Double -> Signal
dft :: Signal -> Signal
idft :: Signal -> Signal
mask :: Int -> Int -> Signal
fft :: Signal -> Signal
ifft :: Signal -> Signal
makeNoise :: Random a => Int -> Int -> a -> a -> Vec a

makeNoise seed n low high =
    let x = mkStdGen seed
        genNum l h = randomR (l, h)
        genList 1 l h g = [c] where (c,y) = genNum l h g
        genList n l h g = c : genList (n-1) l h y where (c,y) = genNum l h g
        in
            Vec $ genList n low high x


length' (Vec x) = length x

rd n = fmap (\x -> fromIntegral (round $ c * x) / c)
    where c = 10^n

absolute = fmap (\(r:+i) -> sqrt(r*r + i*i))

range from to count = map (\x -> from + x * step) [0..count-1]
    where step = (to - from)/count

imagV (Vec a) = Vec $ map (0:+) a
realV (Vec a) = Vec $ map (:+0) a

instance Show a => Show (Vec a) where
    show (Vec lst) = "[" ++ drop 1 lst' ++ "]" 
        where 
            lst' = mconcat $ map (\x -> " " ++ show x) lst

instance Functor Vec where
    fmap f (Vec x) = Vec $ map f x

instance Applicative Vec where
    pure = Vec . repeat
    (Vec f) <*> (Vec x) = Vec $ zipWith ($) f x
    liftA2 f (Vec x) (Vec y) = Vec $ zipWith f x y

instance Foldable Vec where
    foldr f c (Vec a) = Prelude.foldr f c a

instance Semigroup (Vec a) where
    Vec a <> Vec b = Vec $ a ++ b

instance Monoid (Vec a) where
    mempty = Vec []
    
instance Num a => Num (Vec a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    negate = fmap negate
    abs = fmap abs
    signum = fmap signum
    fromInteger x = pure $ fromInteger x

instance (Floating a) => Fractional (Vec a) where
    (/) = liftA2 (/)
    fromRational x = pure $ fromRational x

instance (Floating a) => Floating (Vec a) where
    pi = pure pi
    exp = fmap exp
    log = fmap log
    sin = fmap sin
    cos = fmap cos
    asin = fmap asin
    acos = fmap acos
    atan = fmap atan
    sinh = fmap sinh
    cosh = fmap cosh
    asinh = fmap asinh
    acosh = fmap acosh
    atanh = fmap atanh

mask freq n = realV $ Vec $ one ++ zero ++ one where 
    one = (take freq $ repeat 1)
    zero = (take (n-freq*2) $ repeat 0)


low_pass' :: Int -> Signal -> Signal

low_pass' v sig =
    let
        m = mask v $ length sig
    in idft $ m * dft sig

twiddle n k = 
    let a = [0.. (n - 1)]
        f x = exp (0:+ x * (-2*pi*k / n))
    in Vec $ fmap f a

dft x = 
    let bigN = fromIntegral $ length x -- length of x
        n = Vec [0 .. bigN-1]
        f k = sum $ x * twiddle bigN k
    in fmap f n

idft x = 
    let bigN = fromIntegral $ length x -- length of x
        cX = fmap conjugate x
        n = Vec [0 .. bigN-1]
        diC = (1.0 / bigN) :+ 0
        f k = sum $ cX * twiddle bigN k
    in conj diC $ fmap f n

conj :: Complex Double -> Signal -> Signal
conj b (Vec a) = fmap (\y -> b * conjugate y) (Vec a)

twidd n k = exp( imagV $ k * pure (-2 * pi / fromIntegral n) )
twiddI n k = exp( imagV $ k * pure (2 * pi / fromIntegral n) )

trace_fft m x = trace (m ++ ": " ++ (show $ rd 2 $ absolute x)) x -- debugging helper to print fft results
fft x  
    | n <= 16 = dft x
    | n <= 1 = x
    | otherwise = 
        let (even, odd) = split $ runVec x  
            (e, o) = (fft $ Vec even, fft $ Vec odd) 
            k = Vec [0..]
            p = twidd n k
        in
            (e + p * o) <> (e - p * o)
    
    where 
        n = length x
        split [] = ([], [])
        split [a] = ([a], [])
        split (a:b:c) = (a:x, b:y)
            where (x,y) = split c

ifft x  
    | n <= 16 = idft x
    | n <= 1 = x
    | otherwise = 
        let (even, odd) = split $ runVec x  
            (e, o) = (fft $ Vec even, fft $ Vec odd) 
            -- k = Vec [0..]
            bigN = fromIntegral n
            cE = fmap conjugate e
            cO = fmap conjugate o
            diC = (-1.0 / bigN) :+ 0
            p = twiddI n $ Vec [0..]
        in
            conj diC (e + p * o) <> conj diC (e - p * o)

    where 
        n = length x
        split [] = ([], [])
        split [a] = ([a], [])
        split (a:b:c) = (a:x, b:y)
            where (x,y) = split c


low_pass :: Int -> Signal -> Signal

low_pass v sig =
    let
        m = mask v $ length sig
    in ifft $ m * fft sig