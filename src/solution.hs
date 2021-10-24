{-# LANGUAGE OverloadedStrings #-}

import Data.List
import Data.Maybe
import System.Random

-- https://wiki.haskell.org/99_questions/1_to_10
-- 01 last
myLast :: [a] -> Maybe a
myLast [] = Nothing
myLast [xs] = Just xs
myLast (_:xs) = myLast xs

-- 02 but last
myButLast :: [a] -> Maybe a
myButLast [] = Nothing
myButLast [xs, ys] = Just xs
myButLast (_:xs) = myButLast xs

-- 03 kth
elementAt :: [a] -> Int -> Maybe a
elementAt [] _ = Nothing
elementAt (x:xs) 1 = Just x
elementAt (x:xs) k = elementAt xs (k-1)

-- 04 length
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = 1 + myLength xs


-- 05 reverse
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x: xs) = myReverse xs ++ [x]


-- 06 is palindrome
isPalindrome :: Eq a =>  [a] -> Bool
isPalindrome xs = xs == ys
  where ys = myReverse xs

-- 07 flatten
data NestedList a = Elem a | List [NestedList a]
myFlatten :: NestedList a -> [a]
myFlatten (Elem x) = [x]
myFlatten (List x) = concatMap myFlatten x

-- 08 dedup consecutive
myCompress :: Eq a => [a] -> [a]
myCompress [] = []
myCompress xs@([_]) = xs
myCompress (x:xs)
  | x == y = myCompress (xs)
  | x /= y = [x] ++ myCompress (xs)
  where y = head xs

-- 09 pack
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack (x:xs) = reps : rest
  where reps = (x : takeWhile (==x) xs)
        rest = (pack $ dropWhile (==x) xs)

-- 10 run length encoding
runLength :: Eq a => [a] -> [(Int, a)]
runLength = map (\x -> (length x, head x)) . pack


-- 11 encode modified
data ListItem a = Single a | Multiple Int a
  deriving Show
encodeModified :: Eq a => [a] -> [ListItem a]
encodeModified = map encodeHelper  . runLength
  where
    encodeHelper (1, x) = Single x
    encodeHelper (n, x) = Multiple n x


-- 12 decode
decodeModified :: Eq a => [ListItem a] -> [a]
decodeModified = concatMap decodeHelper
  where
    decodeHelper (Single a) = [a]
    decodeHelper (Multiple n a) = take n $ repeat a


-- 13 run length encoding direct

encodeDirect :: Eq a => [a] -> [ListItem a]
encodeDirect [] = []
encodeDirect (x:xs) = encodeDirectHelper reps : encodeDirect rest
  where
    reps = x: takeWhile (==x) xs
    rest = dropWhile(==x) xs
    encodeDirectHelper [y] = Single x
    encodeDirectHelper ys = Multiple (length ys) (head ys)


-- 14 dup
dupli :: [a] -> [a]
dupli = concatMap (\x -> [x,x])


-- 15 repli
repli :: [a] -> Int -> [a]
repli xs n = concatMap (\x -> take n $ repeat x ) xs

-- 16 drop every Nth
myDrop :: [a] -> Int -> [a]
myDrop [] _ = []
myDrop xs n = firstPart ++ myDrop secondPart n
  where
   firstPart = take (n-1) xs
   secondPart = drop n xs


-- 17 split
mySplit :: [a] -> Int -> [[a]]
mySplit xs n = [(take n xs), (drop n xs)]


-- 18 splice, both ends inclusive
mySlice :: [a] -> Int -> Int -> [a]
mySlice xs start end = take (end - start + 1 ) $ drop (start -1) xs

-- 19 rotate to left
rotate :: [a] -> Int -> [a]
rotate xs n = take len . drop ((n + len) `mod` len) . cycle $ xs
  where len = length xs

-- 20 remove kth
removeAt :: [a] -> Int -> [a]
removeAt xs n = firstPart ++ secondPart
  where
    firstPart = take (n-1) xs
    secondPart = drop n xs

-- 21 insert
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs k = firstPart ++ [x] ++ secondPart
  where
    firstPart = take (k-1) xs
    secondPart = drop (k-1) xs

-- 22 create a list of integer
range :: Int -> Int -> [Int]
range start end = take (end - start + 1) $ iterate (+1) start

-- 23 randomly select
rndSelect :: [a] -> Int -> a
rndSelect = undefined


-- closest timestamps difference
toMinutes :: [Char] -> Maybe Int
toMinutes [a, b ,c, d] = Just $ (read [a,b] :: Int) * 60 + (read [c,d] :: Int)
toMinutes _ = Nothing

zip2 :: [a] -> [(a,a)]
zip2 xs@(x:xTail) = zip xs xTail

appendHead :: [Int] -> [Int]
appendHead xs@(x:xTail) =  xs ++ [x + 24 * 60]

diffhelper :: [String] -> Int
diffhelper = foldl1 min .  map (\(a,b) -> b-a) .  zip2 . appendHead . sort . catMaybes. map toMinutes

--  ["0000","2359", "0800", "0900",  "0601", "jf:jfj"]
smallestDiff :: [String] -> Int
smallestDiff xs
  | len > 24 * 60 = 0
  | len <=1 = error "invalid inputs"
  | otherwise = diffhelper xs
  where len = length xs
