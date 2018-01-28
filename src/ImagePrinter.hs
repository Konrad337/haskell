module ImagePrinter where

import FileReader
import Graphics.Gloss
import Graphics.Gloss.Data.Point
import Graphics.Gloss.Data.Color
import Data.Matrix
import Data.Word
import Data.List
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Environment

-- displays matrix of pixels using gloss lib
printMatrix :: Int -> [Matrix Word8] -> IO ()
printMatrix e mat =  play (InWindow "Nice Window" (800, 800) (300, 300)) white 4 (initWorld e mat 600 300) (\w -> worldPicture w) handle (\f w -> w)

-- makes an image data for using with gloss, notice that all pixels will be displayed as black-white 9x9 square in 10x10 white square
pictureMap :: Matrix Word8 -> Picture
pictureMap mat =
  pictures $
  map
    (\x ->
       color
         (makeColorI (snd x + 90) (snd x + 90) (snd x + 90) 255)
         (polygon [(f x, s x),(f x + 9, s x),(f x + 9, s x + 9),(f x , s x + 9)]))
    [((i, -j), fromIntegral k) | i <- [1 .. (nrows mat)], j <- [1 .. (ncols mat)], k <- [getElem i j mat]]

f x = 10 * fromIntegral (fst $ fst x)
s x =  10 * fromIntegral (snd $ fst x)

data World
            = World
            { worldPicture                  :: Picture
            , pictureNumber                 :: Int
            , worldPictureCollection        :: [Matrix Word8]
            , worldPosX                     :: Float
            , worldPosY                     :: Float

            }


initWorld :: Int -> [Matrix Word8] -> Float -> Float -> World
initWorld e mat wx wy
 = World
        {
         worldPictureCollection = mat,
         pictureNumber = e,
         worldPosX = wx,
         worldPosY = wy,
         worldPicture = translate wx wy $ pictureMap $ mat !! e
        }


handle :: Event -> World -> World
handle event world

        | EventKey (Char 'a') Down _ _ <- event
        = initWorld (pictureNumber world - 1) (worldPictureCollection world) (worldPosX world) (worldPosY world)

        | EventKey (Char 's') Down   _ _  <- event
        = initWorld (pictureNumber world + 1) (worldPictureCollection world) (worldPosX world) (worldPosY world)

        | EventKey (Char 'd') Down   _ _  <- event
        = initWorld (pictureNumber world) (worldPictureCollection world) (worldPosX world + 50) (worldPosY world)

        | EventKey (Char 'f') Down   _ _  <- event
        = initWorld (pictureNumber world ) (worldPictureCollection world) (worldPosX world - 50) (worldPosY world)

        | EventKey (Char 'x') Down   _ _  <- event
        = initWorld (pictureNumber world) (worldPictureCollection world) (worldPosX world) (worldPosY world + 50)

        | EventKey (Char 'c') Down   _ _  <- event
        = initWorld (pictureNumber world ) (worldPictureCollection world) (worldPosX world) (worldPosY world - 50)

        | otherwise
        = world

