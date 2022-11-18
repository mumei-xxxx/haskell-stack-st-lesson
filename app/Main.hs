module Main (main) where

import Lib
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad
import Control.Monad.ST

zeroIndexArray :: UArray Int Bool
zeroIndexArray = array (0,9) [(3,True)]

oneIndexArray :: UArray Int Bool
oneIndexArray = array (1,10) $ zip [1 .. 10] $ cycle [True]

beansInBuckets :: UArray Int Int
beansInBuckets = array (0,3) []

updatedBiB :: UArray Int Int
updatedBiB = beansInBuckets // [(1,5),(3,6)]

listToSTUArray :: [Int] -> ST s (STUArray s Int Int)
listToSTUArray vals = do
    let end =  length vals - 1
    myArray <- newArray (0,end) 0
    forM_ [0 .. end] $ \i -> do
        let val = vals !! i
        writeArray myArray i val
    return myArray

listToUArray :: [Int] -> UArray Int Int
listToUArray vals = runSTUArray $ listToSTUArray vals

main :: IO ()
main = someFunc
