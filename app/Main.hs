module Main where

import Lib

import Graphics.Gloss


main :: IO ()
main = display (InWindow "Nice Window" (200, 200) (10, 10)) white $  color blue $ drawRect $ Rectangle (10, 10) (20, 20)

type Map = [(Int, Int, Field)]

data Field = Empty | Color

data GameState = GameState {
  field :: Map,
  x :: Int,
  y :: Int
}

drawField :: Map -> Picture
drawField field = (>>=) field

drawRect :: Rectangle -> Picture
drawRect rect = translate (fromIntegral px) (fromIntegral py) $ uncurry rectangleSolid (fromIntegral x, fromIntegral y)
  where (x,y) = rectSize rect
        (px, py) = rectPos rect
