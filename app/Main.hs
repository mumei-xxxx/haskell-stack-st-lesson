module Main (main) where

import Lib
import Data.Array.Unboxed
import Data.Array.ST
import Control.Monad
import Control.Monad.ST
import Debug.Trace (trace,traceShowId)

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

-- listToUArray :: [Int] -> UArray Int Int
-- listToUArray vals = runSTUArray $ listToSTUArray vals
{-
ghci> listToUArray [1,2,3]
array (0,2) [(0,1),(1,2),(2,3)]
-}

listToUArray :: [Int] -> UArray Int Int
listToUArray vals = runSTUArray $ do
    let end =  length vals - 1
    myArray <- newArray (0,end) 0
    forM_ [0 .. end] $ \i -> do
        let val = vals !! i
        writeArray myArray i val
    return myArray

{-
ghci> listToUArray [1,2,3]
array (0,2) [(0,1),(1,2),(2,3)]
-}

myData' :: UArray Int Int
myData' = listToUArray [7,6,4,8,10,2]

bubbleSort :: UArray Int Int -> UArray Int Int
bubbleSort myArray = runSTUArray $ do
    stArray <- thaw myArray
    let end = traceShowId((snd . bounds) myArray)
    forM_ [1 .. end] $ \i -> do
        forM_ [0 .. (end - i)] $ \j -> do
            val <- readArray stArray j
            nextVal <- readArray stArray (j + 1)
            let outOfOrder = val > nextVal
            when outOfOrder $ do
                writeArray stArray j nextVal
                writeArray stArray (j + 1) val
    return stArray
{-
ghci> bounds myData'
(0,5) 配列の上限と下限
ghci> myData'
array (0,5) [(0,7),(1,6),(2,4),(3,8),(4,10),(5,2)]
ghci> bubbleSort myData'
array 5
(0,5) [(0,2),(1,4),(2,6),(3,7),(4,8),(5,10)]
-}

main :: IO ()
main = someFunc
