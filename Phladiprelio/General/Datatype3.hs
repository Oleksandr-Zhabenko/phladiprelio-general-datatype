{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE NoImplicitPrelude, BangPatterns #-}

module Phladiprelio.General.Datatype3 (
   Read0
   , isA
   , isB
   , isC 
   , readSimple3
   , basicSplit
   , line2Strings
   , read3
   , readEq4G 
   , readEq4 
) where

import GHC.Base
import GHC.List
import Data.List (groupBy)
import Data.Char (isDigit, isSpace,isLetter)
import Text.Read (readMaybe)
import Text.Show (Show(..))
import GHC.Num ((*),(+))
import Data.Maybe (fromMaybe)
import Data.Tuple (snd)

-- | Is a way to read duration of the additional added time period into the line.
readU2 :: String -> Double
readU2 xs@(y:ys) = fromMaybe 1.0 (readMaybe (y:'.':(if null ys then "0" else ys))::Maybe Double)
readU2 _ = 1.0
{-# INLINE readU2 #-}

splitL0 :: String -> [String]
splitL0 = groupBy (\x y -> (isDigit x && isDigit y) || (x /= '_' && x /= '=' && not (isDigit x) && y /= '_' && y /= '=' && not (isDigit y)) || ((x == '=' || x == '_') && isDigit y))
{-# INLINE splitL0 #-}

data Read0 = A {-# UNPACK #-} !Double | B {-# UNPACK #-} !Double | C String deriving (Eq, Show)

reRead3 :: String -> Read0
reRead3 xs = 
  case splitAt 1 xs of
    ("=",ts) -> A (readU2 ts)
    ("_",ts) -> B (readU2 ts)
    _ -> C xs

isA :: Read0 -> Bool
isA (A _) = True
isA _ = False

isB :: Read0 -> Bool
isB (B _) = True
isB _ = False

isC :: Read0 -> Bool
isC (C _) = True
isC _ = False

filterReads :: [Read0] -> [Read0]
filterReads xs@(B y:A t:us) = B y : filterReads (dropWhile isA us)
filterReads xs@(A y:A t:us) = A y : filterReads (dropWhile isA us)
filterReads xs@(t:ts) = t:filterReads ts
filterReads _ = []

basicSplit :: String -> [Read0]
basicSplit = filterReads . map reRead3 . splitL0
{-# INLINE basicSplit #-}

readSimple3 
  :: (String -> Bool) -- ^ A special function to check whether the 'String' contains needed information. Must return 'True' for the 'String' that contains the needed for usual processment information, otherwise — 'False'.
  -> Double
  -> (String -> [Double])
  -> [Read0]
  -> [Double]
readSimple3 p temp fConvA rs@(C xs:A x:ts) -- This branch is fixed in the version 0.6.0.0 because earlier it has an issue.
 | null qs = readSimple3 p temp fConvA ts
 | null q1 = xl1 : readSimple3 p xl1 fConvA ts
 | otherwise = q1 `mappend` (xl1 : readSimple3 p xl1 fConvA ts)
  where qs 
          | p xs = fConvA xs
          | otherwise = []
        (q1,q2s) = splitAtEnd 1 qs
        ql1 = head q2s
        xl1=x*ql1
readSimple3 p temp fConvA rs@(C xs:ys@(B x:ts)) = qs `mappend` qqs `mappend` readSimple3 p ql fConvA ws  
  where (ks, ws) = span isB ys
        qs 
          | p xs = fConvA xs
          | otherwise = []
        ql
          | null qs = 0.0
          | otherwise = last qs
        qqs = map (\(B k) -> k * ql) ks
readSimple3 p temp fConvA rs@(B x:ts) = qqs `mappend` readSimple3 p temp fConvA ws 
  where (ks, ws) = span isB rs
        qqs = map (\(B k) -> k * temp) ks
readSimple3 p temp fConvA [C xs] = qs
  where qs 
          | p xs = fConvA xs
          | otherwise = []
readSimple3 _ _ _ _ = []
{-# INLiNABLE readSimple3 #-}

read3 
 :: (String -> Bool) -- ^ A special function to check whether the 'String' contains needed information. Must return 'True' for the 'String' that contains the needed for usual processment information, otherwise — 'False'.
 -> Double
 -> (String -> [Double])
 -> String 
 -> [Double]
read3 p temp fConvA = filter (/= 0.0) . readSimple3 p temp fConvA . basicSplit
{-# INLINE read3 #-}

splitAtEnd :: Int -> [a] -> ([a], [a])
splitAtEnd n = (\(x,y,_,_) -> (y,x)) . foldr f v
 where v = ([],[],0,n)
       f x (zs,ts,k,n)
        | k < n = (x : zs,[],k + 1,n)
        | otherwise = (zs,x : ts,k + 1,n)

-- | Is a specialized version of 'Data.SubG.dropFromEndG' function variant from the @subG@ package. Is taken from there to
-- reduce the dependencies. Is not intended to be exported at all.
dropFromEnd :: Int -> [a] -> [a]
dropFromEnd n = (\(xs,_,_) -> xs) . foldr f v
 where v = ([],0,n)
       f x (zs,k,n)
        | k < n = ([],k + 1,n)
        | otherwise = (x : zs,k,n)

line2Strings 
 :: (String -> Bool) -- ^ A special function to check whether the 'String' contains needed information. Must return 'True' for the 'String' that contains the needed for usual processment information, otherwise — 'False'.
 -> (String -> [String])
 -> [Read0]
 -> [String]
line2Strings p gConvC xs@(C ts:tt@(A x):ys) = ks `mappend` ((ql `mappend` (if null ql then [] else '=':showRead0AsInsert tt)) : line2Strings p gConvC ys) 
  where (ks, qs) 
          | p ts = splitAtEnd 1 . gConvC $ ts 
          | otherwise = ([],[])
        ql 
          | null qs = []
          | otherwise = head qs
line2Strings p gConvC xs@(C ys:ts) = gConvC ys `mappend` line2Strings p gConvC ts
line2Strings p gConvC xs@(y@(B x):ts) = showRead0AsInsert y : line2Strings p gConvC ts
line2Strings _ _ _ = []
{-# INLINABLE line2Strings #-}

-- | Is intended to be used in the "music" mode for PhLADiPreLiO.
readEq4G 
 :: (String -> Bool) -- ^ A special function to check whether the 'String' contains needed information. Must return 'True' for the 'String' that contains the needed for usual processment information, otherwise — 'False'.
 -> (String -> [Double])
 -> (String -> [String])
 -> [Read0]
 -> [(String, Double)]
readEq4G p fConvA gConvC xs = zip ks rs
   where ks = line2Strings p gConvC xs
         rs = filter (/= 0.0) . readSimple3 p 1.0 fConvA $ xs
{-# INLINABLE readEq4G #-}

readEq4
 :: (String -> [Double])
 -> (String -> [String])
 -> [Read0]
 -> [(String, Double)]
readEq4 = readEq4G (not . null . filter (not . isSpace))
{-# INLINE readEq4 #-}

showRead0AsInsert :: Read0 -> String
showRead0AsInsert d@(A t) = '=':(filter (/= '.') . show $ t)
showRead0AsInsert d@(B t) = '_':(filter (/= '.') . show $ t)
showRead0AsInsert d@(C ts) = ts
{-# INLINE showRead0AsInsert #-}

