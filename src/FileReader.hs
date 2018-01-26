module FileReader where

import Data.Matrix
import Data.Word
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BLC
import System.IO as IO

imageFileSource :: String
imageFileSource = "/home/konrad/IdeaProjects/haskell/t10k-images-idx3-ubyte"

labelFileSource :: String
labelFileSource = "/home/konrad/IdeaProjects/haskell/t10k-labels-idx1-ubyte"

getFileContents :: FilePath  -> IO B.ByteString
getFileContents = B.readFile

checkMagicNumber :: B.ByteString -> Bool
checkMagicNumber bytesToCheck = bytesToCheck == B.pack [0x00, 0x00, 0x08, 0x03]
--returns True if 4 bytes match mn




readImageFile :: IO[Matrix Word8]
readImageFile = do
  contents <- getFileContents imageFileSource
  let magicNumber = fst $ B.splitAt 4 contents
  let contents' = snd $ B.splitAt 4 contents
  Prelude.putStrLn "Checking magic number"
  if not (checkMagicNumber magicNumber)
    then Prelude.putStrLn "Magic number not correct" >> return []
    else do
      Prelude.putStrLn "Magic number correct"
      let numberOfImages = B.last $ fst $ B.splitAt 4 contents'
      let contents'' = snd $ B.splitAt 4 contents'
      let numberOfRows = B.last $ fst $ B.splitAt 4 contents''
      let contents''' = snd $ B.splitAt 4 contents''
      let numberOfColumns = B.last $ fst $ B.splitAt 4 contents'''
      let contents'''' = snd $ B.splitAt 4 contents'''
      return $ translatePixelsToMatrix contents'''' (fromIntegral numberOfRows) (fromIntegral numberOfColumns) [] 1



translatePixelsToMatrix :: B.ByteString -> Int -> Int -> [Matrix Word8] -> Int -> [Matrix Word8]
translatePixelsToMatrix bytes columns rows mat it = do
  let contents = B.drop (toEnum (rows * columns * it)) bytes
  if not $ B.null contents
      then do
      let mat' = matrix rows columns (\(i, j) -> (B.index bytes $ toEnum $ ((i - 1) + (j - 1)*rows) + it*rows*columns )) : mat
      translatePixelsToMatrix bytes rows columns mat' (it+1)
  else mat






