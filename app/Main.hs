{-# LANGUAGE BlockArguments #-}

module Main where

import Graphics.Gloss
import Lib

main :: IO ()
main = display (InWindow "TetrisHS" (200, 200) (10, 10)) white $ drawField initField

type Pos = (Int, Int)
type Map = [(Pos, Color)]
type Tetris = Map
type Shape = [Pos]

data GameState = GameState
  { field :: Map,
    tetris :: Tetris,
    pos :: Pos
  }

drawField :: Map -> Picture
drawField field = Pictures $ (\((x, y), c) -> color c (drawRect (Rectangle (x * 10, y * 10) (10, 10)))) <$> field

initField :: Map
initField = [((-1,y), black) | y <- [0 .. 20] ] ++
            [((11,y), black) | y <- [0 .. 20] ] ++
            [((x,0), black) | x <- [0 .. 10] ]

drawRect :: Rectangle -> Picture
drawRect rect = translate (fromIntegral px) (fromIntegral py) $ uncurry rectangleSolid (fromIntegral x, fromIntegral y)
  where
    (x, y) = rectSize rect
    (px, py) = rectPos rect

colorizeShape :: Shape -> Color -> Tetris
colorizeShape s c = (\(x, y) -> ((x, y), c)) <$> s

tetriz :: [Tetris]
tetriz = [
  colorizeShape l $ bright red,
  colorizeShape (mirrored l) $ bright blue,
  colorizeShape z $ bright green,
  colorizeShape (mirrored z) $ bright magenta,
  colorizeShape i $ bright cyan,
  colorizeShape i $ bright orange]

l :: Shape
l = [(0, 0), (0, 1), (0, 2), (1, 0)]

z :: Shape
z = [(0, 0), (0, 1), (1, 1), (1, 2)]

i :: Shape
i = [(0, 0), (0, 1), (0, 2), (0, 3)]

o :: Shape
o = [(0, 0), (0, 1), (1, 0), (1, 1)]

mirrored :: Shape -> Shape
mirrored s = (\(x, y) -> (- x, y)) <$> s

rotate :: Shape -> Shape
rotate s = (\(x, y) -> (- y, x)) <$> s

getShape :: Map -> Shape
getShape m = fst <$> m

colides :: Shape -> Shape -> Bool
colides m t = any (\(x, y) -> (x, y) `elem` t) m

translateShape :: Shape -> Pos -> Shape
translateShape s (x1, y1) = (\(x, y) -> (x + x1, y + y1)) <$> s

anker :: Map -> Tetris -> Map
anker m t = m ++ t

getRandomTetris :: Tetris
getRandomTetris = head tetriz

step :: GameState -> GameState
step (GameState m t (x, y)) =
  GameState
    (if colidesWithMapWhenDrop then anker m t else m)
    (if colidesWithMapWhenDrop then getRandomTetris else t)
    (if colidesWithMapWhenDrop then (3, 21) else (x, y - 1))
  where
    colidesWithMapWhenDrop = colides (translateShape (getShape t) (x, y - 1)) (getShape m)
