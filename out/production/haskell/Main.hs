module Main where

import ImagePrinter
import FileReader
import Data.Matrix
import Data.Word

main :: IO ()
main = readImageFile >>= printMatrix 0
