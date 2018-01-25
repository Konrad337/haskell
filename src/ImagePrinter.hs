module ImagePrinter where

import FileReader
import Graphics.Gloss
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Color
import Data.Matrix
import Data.Word
import Data.List


printMatrix :: Int -> [Matrix Word8] -> IO ()
printMatrix e mat = display (InWindow "Nice Window" (400, 400) (600, 300)) white (pictureMap $ mat !! e)


pictureMap :: Matrix Word8 -> Picture
pictureMap mat =
  pictures $
  map
    (\x ->
       color
         (makeColorI (0) (snd x + 100) (snd x + 100) 255)
         (polygon [(f x, s x),(f x + 10, s x),(f x, s x + 10),(f x + 10, s x + 10)]))
    [((-i, -j), fromIntegral k) | i <- [1 .. (nrows mat)], j <- [1 .. (ncols mat)], k <- [getElem i j mat]]

f x = 10 * fromIntegral (fst $ fst x)
s x = 10 * fromIntegral (snd $ fst x)

