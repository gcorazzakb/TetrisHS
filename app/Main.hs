{-# LANGUAGE BlockArguments #-}

module Main where

import Graphics.Gloss
import Lib
import Graphics.Gloss.Data.ViewPort (ViewPort)
import Graphics.Gloss.Interface.IO.Game (Event(EventKey), SpecialKey (..), Key (..), KeyState (Down))
import System.Random (getStdRandom, newStdGen, randomR, StdGen)
import Debug.Trace (traceShow)

main :: IO ()
main = do
      randomGen <- newStdGen
      play
        (InWindow "TetrisHS" (300, 500) (10, 10))
        white
        60
        (initGame randomGen)
        drawGame
        handleInput
        step

type Pos = (Int, Int)
type Tetris = [(Pos, Color)]
type Shape = [Pos]
fallTime = 0.5

data GameModel = GameModel
  { field :: Tetris,
    tetris :: Tetris,
    pos :: Pos,
    lastFalltime :: Float,
    rand :: StdGen
  }

drawTetris :: Tetris -> Picture
drawTetris t = Pictures $ (\((x, y), c) -> color c (drawRect (Rectangle (x * 10, y * 10) (10, 10)))) <$> t

drawGame :: GameModel -> Picture
drawGame (GameModel m t (x,y) _ _) = translate (-50) (-30) $ Pictures [drawTetris m,
                                                                     translate (fromIntegral x*10 ) (fromIntegral y*10) (drawTetris t)]

initGame :: StdGen -> GameModel
initGame rand = GameModel initField (fst (randomTetris rand)) (4,20) 0 rand

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

randomTetris :: StdGen -> (Tetris, StdGen)
randomTetris rand = (tetriz !! fst randT, snd randT)
        where randT = randomR (0, (length tetriz) - 1) rand

step :: Float -> GameModel -> GameModel
step s (GameModel m t p ft r) = if (ft + s) > fallTime then fst $ fall (GameModel m t p ft r)
                                                     else GameModel m t p (ft + s + s) r

fall :: GameModel -> (GameModel, Bool)
fall (GameModel m t (x, y) s r) =
   (GameModel
    (if colidesWithMapAfterDrop then deleteFullLines $ anker m t (x,y) else m)
    (if colidesWithMapAfterDrop then fst randTet else t)
    (if colidesWithMapAfterDrop then (3, 21) else (x, y - 1))
    0
    (snd randTet)
    ,
    colidesWithMapAfterDrop)
  where
    colidesWithMapAfterDrop = collisionState (GameModel m t (x, y - 1) s r)
    randTet = randomTetris r

collisionState :: GameModel -> Bool
collisionState (GameModel m t (x, y) _ _) = colides (translateShape (getShape t) (x, y)) (getShape m)

fastfall :: GameModel -> GameModel
fastfall g = fst $ until snd (\(g, _) -> fall g) (g, False)

move :: GameModel -> Pos -> GameModel
move (GameModel m t (x, y) s r) (sx,sy)= if collisionState transG
                                then g
                                else transG
                                where g = GameModel m t (x, y) s r
                                      transG = GameModel m t (x + sx, y + sy) s r

rotateKey :: GameModel -> GameModel
rotateKey (GameModel m t p s r) =  if collisionState rotG
                                      then GameModel m t p s r
                                      else rotG
                                      where rotG = GameModel m (rotateTetris t) p s r

elems :: (Eq a) => [a] -> [a] -> Bool
elems es l = all (`elem` l) es

deleteFullLines :: Tetris -> Tetris
deleteFullLines m = foldl deleteLine m ((>>=) [1 .. 19] (\z -> ([z | isLineFull m z])))

isLineFull :: Tetris -> Int -> Bool
isLineFull m y = traceShow a a where a = elems [(x, y) | x <- [0 .. 10]] $ fst <$> m

deleteLine :: Tetris -> Int -> Tetris
deleteLine m i =(\((x,y),c) -> if y > i && x /= -1 && x /= 11
                               then ((x,y - 1),c)
                               else ((x,y),c))
                                                <$> filter (\((x,y),_) -> y /= i || isWall (x, y)) m

isWall :: Pos -> Bool
isWall (x,y) = x == -1 || x == 11 || y == 0

handleInput :: (Event -> GameModel -> GameModel)
handleInput (EventKey key Down _ _) g = case key of SpecialKey KeyUp -> fastfall g
                                                    SpecialKey KeyDown -> fst $ fall g
                                                    SpecialKey KeyLeft -> move g (-1,0)
                                                    SpecialKey KeyRight -> move g (1,0)
                                                    SpecialKey KeySpace -> rotateKey g
                                                    _ -> g
handleInput _ g = g
