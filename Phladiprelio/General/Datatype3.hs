{-# OPTIONS_GHC -funbox-strict-fields #-}
{-# LANGUAGE NoImplicitPrelude, BangPatterns #-}

module Phladiprelio.General.Datatype3 (
   Read0
   , isA
   , isB
   , isC 
   , readU2
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
import qualified Data.Foldable as F (foldr) 
import qualified Data.Sequence as S

-- | Is a way to read duration of the additional added time period into the line.
readU2 :: String -> Double
readU2 xs@(y:ys) = fromMaybe 1.0 (readMaybe (y:'.':(if null ys then "0" else ys))::Maybe Double)
readU2 _ = 1.0
{-# INLINE readU2 #-}

-- | Splits a 'String' into list of 'String' so that they can be read by other functions here into respective datatypes.
splitL0 :: String -> [String]
splitL0 = groupBy (\x y -> (isDigit x && isDigit y) || (x /= '_' && x /= '=' && not (isDigit x) && y /= '_' && y /= '=' && not (isDigit y)) || ((x == '=' || x == '_') && isDigit y))
{-# INLINE splitL0 #-}

data Read0 = A {-# UNPACK #-} !Double | B {-# UNPACK #-} !Double | C String deriving (Eq, Show)

-- | Converts a specially formatted 'String' into a 'Read0' value.
reRead3 :: String -> Read0
reRead3 xs = 
  case uncons xs of
    Just ('=',ts) -> A (readU2 ts)
    Just ('_',ts) -> B (readU2 ts)
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

filterReads :: [Read0] -> S.Seq Read0
filterReads xs@(B y:A t:us) = B y S.<| filterReads (dropWhile isA us)
filterReads xs@(A y:A t:us) = A y S.<| filterReads (dropWhile isA us)
filterReads xs@(t:ts) = t S.<| filterReads ts
filterReads _ = S.empty

-- | A preparatory function for the further ones here.
basicSplit :: String -> S.Seq Read0
basicSplit = filterReads . map reRead3 . splitL0
{-# INLINE basicSplit #-}

readSimple3 
  :: (String -> Bool) -- ^ A special function to check whether the 'String' contains needed information. Must return 'True' for the 'String' that contains the needed for usual processment information, otherwise — 'False'.
  -> Double
  -> (String -> [Double])
  -> S.Seq Read0 -- ^ Is should be obtained using 'basicSplit' function here.
  -> [Double]
readSimple3 p temp fConvA rs@(C xs S.:<| A x S.:<| ts) -- This branch is fixed in the version 0.6.0.0 because earlier it has an issue.
 | null qs = readSimple3 p temp fConvA ts
 | null q1 = xl1 : readSimple3 p xl1 fConvA ts
 | otherwise = q1 `mappend` (xl1 : readSimple3 p xl1 fConvA ts)
  where qs 
          | p xs = fConvA xs
          | otherwise = []
        (q1,q2s) = splitAtEnd 1 qs
        ql1 = head q2s
        xl1=x*ql1
readSimple3 p temp fConvA rs@(C xs S.:<| ys@(B x S.:<| ts)) = qs `mappend` qqs `mappend` readSimple3 p ql fConvA ws  
  where (ks, ws) = S.spanl isB ys
        qs 
          | p xs = fConvA xs
          | otherwise = []
        ql
          | null qs = 0.0
          | otherwise = last qs
        qqs = F.foldr (\(B k) js -> k * ql:js) [] ks
readSimple3 p temp fConvA rs@(B x S.:<| ts) = qqs `mappend` readSimple3 p temp fConvA ws 
  where (ks, ws) = S.spanl isB rs
        qqs = F.foldr (\(B k) js -> k * temp:js) [] ks
readSimple3 p temp fConvA (C xs S.:<| _) = qs
  where qs 
          | p xs = fConvA xs
          | otherwise = []
readSimple3 _ _ _ _ = []

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

-- | Is a specialized version of 'Data.InsertLeft.dropFromEndG' function variant from the @subG@ package. Is taken from there to
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
 -> S.Seq Read0 -- ^ Is should be obtained using 'basicSplit' function here.
 -> [String]
line2Strings p gConvC xs@(C ts S.:<| tt@(A x) S.:<| ys) 
 | null qs = ks `mappend` line2Strings p gConvC ys
 | otherwise = ks `mappend` ((ql `mappend` ('=':showRead0AsInsert tt)) : line2Strings p gConvC ys) 
  where (ks, qs) 
          | p ts = splitAtEnd 1 . gConvC $ ts 
          | otherwise = ([],[])
        ql = head qs
line2Strings p gConvC xs@(C ys S.:<| ts) = gConvC ys `mappend` line2Strings p gConvC ts
line2Strings p gConvC xs@(y@(B x) S.:<| ts) = showRead0AsInsert y : line2Strings p gConvC ts
line2Strings _ _ _ = []

-- | Is intended to be used in the "music" mode for PhLADiPreLiO.
readEq4G 
 :: (String -> Bool) -- ^ A special function to check whether the 'String' contains needed information. Must return 'True' for the 'String' that contains the needed for usual processment information, otherwise — 'False'.
 -> (String -> [Double])
 -> (String -> [String])
 -> S.Seq Read0 -- ^ Is should be obtained using 'basicSplit' function here.
 -> [(String, Double)]
readEq4G p fConvA gConvC xs = zip ks rs
   where ks = line2Strings p gConvC xs
         rs = filter (/= 0.0) . readSimple3 p 1.0 fConvA $ xs
{-# INLINE readEq4G #-}

readEq4
 :: (String -> [Double])
 -> (String -> [String])
 -> S.Seq Read0 -- ^ Is should be obtained using 'basicSplit' function here.
 -> [(String, Double)]
readEq4 = readEq4G (not . null . filter (not . isSpace))
{-# INLINE readEq4 #-}

showRead0AsInsert :: Read0 -> String
showRead0AsInsert d@(A t) = '=':(filter (/= '.') . show $ t)
showRead0AsInsert d@(B t) = '_':(filter (/= '.') . show $ t)
showRead0AsInsert d@(C ts) = ts
{-# INLINE showRead0AsInsert #-}

