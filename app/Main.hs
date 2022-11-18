module Main (main) where

import Lib
import Data.Array.Unboxed

zeroIndexArray :: UArray Int Bool
zeroIndexArray = array (0,9) [(3,True)]

oneIndexArray :: UArray Int Bool
oneIndexArray = array (1,10) $ zip [1 .. 10] $ cycle [True]

beansInBuckets :: UArray Int Int
beansInBuckets = array (0,3) []

updatedBiB :: UArray Int Int
updatedBiB = beansInBuckets // [(1,5),(3,6)]

main :: IO ()
main = someFunc
