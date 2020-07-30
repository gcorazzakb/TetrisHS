{-# LANGUAGE BlockArguments #-}

module Main where

import Graphics.Gloss
import Lib
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Graphics.Gloss.Interface.IO.Game (Event(EventKey), SpecialKey (..), Key (..), KeyState (Down))
import Debug.Trace (traceShow)

main :: IO ()
main = Graphics.Gloss.play
  (InWindow "TetrisHS" (300, 500) (10, 10))
  white
  60
  initGame
  drawGame
  handleInput
  step

type Pos = (Int, Int)
type Tetris = [(Pos, Color)]
type Shape = [Pos]
fallTime = 0.5

data GameState = GameState
  { field :: Tetris,
    tetris :: Tetris,
    pos :: Pos,
    lastFalltime :: Float
  }

drawTetris :: Tetris -> Picture
drawTetris t = Pictures $ (\((x, y), c) -> color c (drawRect (Rectangle (x * 10, y * 10) (10, 10)))) <$> t

drawGame :: GameState -> Picture
drawGame (GameState m t (x,y) _) = translate (-50) (-30) $ Pictures [drawTetris m,
                                                                     translate (fromIntegral x*10 ) (fromIntegral y*10) (drawTetris t)]

initGame :: GameState
initGame = GameState initField getRandomTetris (4,20) 0

initField :: Tetris
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
  colorizeShape o $ bright orange,
  colorizeShape t $ bright yellow
  ]

l :: Shape
l = [(0, 0), (0, 1), (0, 2), (1, 0)]

z :: Shape
z = [(0, 0), (0, 1), (1, 1), (1, 2)]

i :: Shape
i = [(0, 0), (0, 1), (0, 2), (0, 3)]

o :: Shape
o = [(0, 0), (0, 1), (1, 0), (1, 1)]

t :: Shape
t = [(0, 1), (1, 1), (2, 1), (1, 0)]

mirrored :: Shape -> Shape
mirrored s = (\(x, y) -> (- x, y)) <$> s

rotateTetris :: Tetris -> Tetris
rotateTetris s = (\((x, y), c) -> ((-y, x), c)) <$> s

getShape :: Tetris -> Shape
getShape m = fst <$> m

colides :: Shape -> Shape -> Bool
colides m t = any (\(x, y) -> (x, y) `elem` t) m

translateShape :: Shape -> Pos -> Shape
translateShape s (x1, y1) = (\(x, y) -> (x + x1, y + y1)) <$> s

translateTetris :: Tetris -> Pos -> Tetris
translateTetris s (x1, y1) = (\((x, y), c) -> ((x + x1, y + y1), c)) <$> s

anker :: Tetris -> Tetris -> Pos -> Tetris
anker m t p= m ++ translateTetris t p

getRandomTetris :: Tetris
getRandomTetris = head tetriz

step :: Float -> GameState -> GameState
step s (GameState m t p ft) = if (ft + s) > fallTime then fst $ fall (GameState m t p ft)
                                                     else GameState m t p (ft + s + s)
fall :: GameState -> (GameState, Bool)
fall (GameState m t (x, y) s) =
   (GameState
    (if colidesWithMapAfterDrop then anker m t (x,y) else m)
    (if colidesWithMapAfterDrop then getRandomTetris else t)
    (if colidesWithMapAfterDrop then (3, 21) else (x, y - 1))
    0
    ,
    colidesWithMapAfterDrop)
  where
    colidesWithMapAfterDrop = collisionState (GameState m t (x, y - 1) s)

collisionState :: GameState -> Bool
collisionState (GameState m t (x, y) _) = colides (translateShape (getShape t) (x, y)) (getShape m)

fastfall :: GameState -> GameState
fastfall g = fst $ until snd (\(g, _) -> fall g) (g, True)

move :: GameState -> Pos -> GameState
move (GameState m t (x, y) s) (sx,sy)= if collisionState transG
                                then g
                                else transG
                                where g = GameState m t (x, y) s
                                      transG = GameState m t (x + sx, y + sy) s

rotateKey :: GameState -> GameState
rotateKey (GameState m t p s) =  if collisionState rotG
                                      then GameState m t p s
                                      else rotG
                                      where rotG = GameState m (rotateTetris t) p s

handleInput :: (Event -> GameState -> GameState)
handleInput (EventKey key Down _ _) g = case key of SpecialKey KeyUp -> fastfall g
                                                    SpecialKey KeyDown -> fst $ fall g
                                                    SpecialKey KeyLeft -> move g (-1,0)
                                                    SpecialKey KeyRight -> move g (1,0)
                                                    SpecialKey KeySpace -> rotateKey g
                                                    _ -> g
handleInput _ g = g
