module Main where

import Lib

import Graphics.Gloss


main :: IO ()
main = display (InWindow "Nice Window" (200, 200) (10, 10)) white $ drawField initField

type Pos = (Int, Int)
type Map = [(Pos, Field)]
type Tetris = Map
type Field = Color
type Shape = [Pos]

data GameState = GameState {
  field :: Map,
  tetris :: Tetris,
  x :: Int,
  y :: Int
}

drawField :: Map -> Picture
drawField field = Pictures $ (\((x,y),c) -> color c (drawRect (Rectangle (x* 10,y * 10) (10,10)))) <$> field

{-filterEmptyFields :: Map -> Map
filterEmptyFields map = (>>=) map (\(x,y,f) -> case f of Nothing -> []
                                                         f  -> [(x,y,f)])  -}

initField :: Map
initField = [((1,1),blue),((1,2),blue), ((1,3),red), ((2,1),red)]

drawRect :: Rectangle -> Picture
drawRect rect = translate (fromIntegral px) (fromIntegral py) $ uncurry rectangleSolid (fromIntegral x, fromIntegral y)
  where (x,y) = rectSize rect
        (px, py) = rectPos rect

colorizeShape :: Shape -> Color -> Tetris
colorizeShape s c = (\(x,y) -> ((x,y),c)) <$> s

l :: Shape
l = [(0,0), (0,1), (0,2), (1,0)]

z :: Shape
z = [(0,0), (0,1), (1,1), (1,2)]

i :: Shape
i = [(0,0), (0,1), (0,2), (0,3)]

o :: Shape
o = [(0,0), (0,1), (1,0), (1,1)]

mirrored :: Shape -> Shape
mirrored s = (\(x,y) -> (-x,y)) <$> s
