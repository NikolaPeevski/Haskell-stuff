module Hw6 
    (maxList
    , maxList'
    , reorder
    , bubble_sort)
    where

-- import Random

maxList :: [Int] -> Int
maxList' :: (a -> a -> Bool) -> [a] -> a
reorder :: [Int] -> (Bool, [Int])
bubble_sort :: [Int] -> [Int]

reorder [] = (False, [])
reorder [a] = (False, [a])
reorder (a:b:c) =
    if a > b 
        then
            let (z, x) = reorder (a:c) in
            (True, b : x)
        else 
            let (z, x) = reorder (b:c) in
            (z, a : x)
            
bubble_sort a =
    let (x, y) = reorder a in
    if x then
        bubble_sort y
    else y

maxList = maximum

maxList' f [a] = a
maxList' f (a:b:c) = 
    if f a b 
        then maxList' f (a:c) 
        else maxList' f (b:c) 