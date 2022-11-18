module Main (main) where

import Lib
import Data.Array.Unboxed

zeroIndexArray :: UArray Int Bool
zeroIndexArray = array (0,9) [(3,True)]

oneIndexArray :: UArray Int Bool
oneIndexArray = array (1,10) $ zip [1 .. 10] $ cycle [True]

main :: IO ()
main = someFunc
