module Lib
    ( mainApp
    ) where

import qualified SDL
import qualified Common as C

import qualified Data.Map as Map
import Data.Foldable          (foldl')
import Foreign.C.Types

type Coord = (CInt, CInt)
type Position = (CInt, CInt)

radius :: CInt
radius = 23 :: CInt

-- Intents
data Intent
  = Idle
  | Quit
  | Move Coord
  | Press Coord
  | Release Coord

data Stone = Stone
  { color :: Bool -- True is black, False is white
  , pos :: Position
  , coord :: Coord
  , sticky :: Bool -- if the mouse is having the stone
  , captured :: Coord -- distance from the mouse to the stone
  } deriving (Eq)

data Bowl = Bowl
  { coordB :: Coord
  , stickyB :: Bool -- if the mouse is having the stone
  } deriving (Eq)

-- bowl of the stones which we take the stones from
data Bowls = Bowls
  { black :: Bowl
  , white :: Bowl
  } deriving (Eq)

data StoneState = Black
                | White
                | Ko
                | Empty
                deriving (Eq, Show)

data StoneMove = Pass StoneState
               | StoneMove Position StoneState
               deriving (Eq)

-- The world
data World = World
  { exiting :: Bool
  , bowls :: Bowls
  , stones :: [Stone]
  , board :: Map.Map Position StoneState
  , lastMove :: StoneMove
  } deriving (Eq)

initialWorld :: World
initialWorld = World
  { exiting = False
  , bowls = Bowls { black = Bowl { coordB = (530, 65), stickyB = False }
                  , white = Bowl { coordB = (530, 115), stickyB = False }
                  }
  , stones = []
  , board = addPieces (Map.empty) points
  , lastMove = StoneMove (-1,-1) White
  } where points = [(x,y) | x <- [0..8], y <- [0..8]]

addPieces :: (Map.Map Position StoneState) -> [Position] -> (Map.Map Position StoneState)
addPieces m [] = m
addPieces m (x:xs) = addPieces (Map.insert x Empty m) xs

-- Main entry to our application logic. It takes the handle to the SDL Window,
-- sets everything up and executes the main application loop: handle user inputs,
-- and draw the world.
mainApp :: SDL.Window -> IO ()
mainApp win =
    C.withRenderer win $ \r -> do
      tBackground <- C.loadTextureWithInfo r "./assets/wood.png"
      tBlackStone <- C.loadTextureWithInfo r "./assets/b.png"
      tWhiteStone <- C.loadTextureWithInfo r "./assets/w.png"
      let t = [tBackground, tBlackStone, tWhiteStone]

      -- obtain the size of the window
      mSurface <- SDL.getWindowSurface win
      (SDL.V2 winWidth winHeight) <- SDL.surfaceDimensions mSurface

      -- we create an utility curry for us here
      let doRender = Lib.renderWorld r t (fromIntegral winWidth, fromIntegral winHeight)

      let loop w = if Lib.exiting w
                      then return ()
                      else do
                              events <- SDL.pollEvents
                              let newWorld = updateWorld w events
                              doRender newWorld
                              loop newWorld

      loop Lib.initialWorld

      -- when we are done with the renderer, we need to clean up
      mapM_ (SDL.destroyTexture . fst) t

-- Given a list of events, update the world
updateWorld :: World -> [SDL.Event] -> World
updateWorld w
  = foldl' (flip applyIntent) w
  . fmap (payloadToIntent . SDL.eventPayload)


-- Convert the SDL event to Intent
payloadToIntent :: SDL.EventPayload -> Intent
payloadToIntent SDL.QuitEvent            = Quit -- window CLOSE pressed
payloadToIntent (SDL.KeyboardEvent e)    = -- when Q is pressed, quit also
  if SDL.keysymKeycode (SDL.keyboardEventKeysym e) == SDL.KeycodeQ then Quit else Idle
payloadToIntent (SDL.MouseButtonEvent e) = buttonIntent e -- mouse button pressed
payloadToIntent (SDL.MouseMotionEvent e) = motionIntent e -- mouse moved
payloadToIntent _                        = Idle

-- Convert mouse button event to Intent
buttonIntent :: SDL.MouseButtonEventData -> Intent
buttonIntent e =
  let (SDL.P (SDL.V2 x y)) = SDL.mouseButtonEventPos e
  in if SDL.mouseButtonEventMotion e == SDL.Pressed
        -- pass at where the mouse is pressed/released
        then Press (fromIntegral x, fromIntegral y)
        else Release (fromIntegral x, fromIntegral y)

-- Convert mouse motion event to Intent
motionIntent :: SDL.MouseMotionEventData -> Intent
motionIntent e =
  let (SDL.P (SDL.V2 x y)) = SDL.mouseMotionEventPos e
  -- pass the position of the mouse when the mouse is moved
  in Move (fromIntegral x, fromIntegral y)

-- Notice the point-free notation - we have defined our function without referencing the last argument World
applyIntent :: Intent -> World -> World
applyIntent Quit        = quitWorld
applyIntent (Press p)   = pressMouse p
applyIntent (Release p)   = releaseMouse p
applyIntent (Move p)    = moveObject p
applyIntent Idle        = idleWorld

-- let's exit
quitWorld :: World -> World
quitWorld w = w { exiting = True }

-- calculate real position from coordinate index
-- TODO: assert (x,y) is in the range
calculatePos :: Position -> Coord
calculatePos (x,y) = (40+x*50, 40+y*50)

-- takes the position of the mouse, updates the status of a stone if the mouse is on the stone
-- doesn't change the position of the stone
pressMouse :: Coord -> World -> World
pressMouse (x, y) w =
  -- takes the current stones, adds a new stone to the list if the mouse is on the bowl, captures the new stone
  let newStone :: World -> [Stone]
      newStone (World _ bowl _ _ lm)
        -- if the mouse is on the bowl of the turn
        -- the pos field doesn't matter because it uses coord to place the stone,
        -- it will set pos when the mouse is released
        | side == Black && distanceB < radius' =
          Stone { color = True, pos = (0,0), coord = (bx,by), sticky = True, captured = (x-bx, y-by) } : []
        | side == White && distanceW < radius' =
          Stone { color = False, pos = (0,0), coord = (wx,wy), sticky = True, captured = (x-wx, y-wy) } : []
        | otherwise = []
        where (bx, by) = coordB . black $ bowl
              (wx, wy) = coordB . white $ bowl
              distanceB = sqrt (fromIntegral $ (x-bx)^(2::Int) + (y-by)^(2::Int))
              distanceW = sqrt (fromIntegral $ (x-wx)^(2::Int) + (y-wy)^(2::Int))
              side = getSideFromLastMove lm
              radius' = fromIntegral radius::Double
  in w { stones = newStone w }

-- takes the position of the mouse,
-- updates the stone if it is sticky: move the position following the mouse
moveObject :: Coord -> World -> World
moveObject (x,y) w =
  let updatePos :: [Stone] -> [Stone]
      updatePos [] = []
      updatePos (s:ss) = if sticky s
                            then s { coord = (x - fst (captured s), y - snd (captured s)) } : updatePos ss
                            else s : updatePos ss
  in w { stones = updatePos $ stones w }

-- returns opposite StoneState of given StoneState
getOppositeStone :: StoneState -> StoneState
getOppositeStone stone | stone == Black = White
                      | stone == White = Black
                      | otherwise = Empty

-- returns opposite stone from the lastMove
getSideFromLastMove :: StoneMove -> StoneState
getSideFromLastMove (StoneMove _ st) = getOppositeStone st
getSideFromLastMove (Pass st) = getOppositeStone st

-- takes world and position returns the StoneState
seekBoard :: World -> Position -> StoneState
seekBoard (World _ _ _ m _) p = case Map.lookup p m of
    Just stoneState -> stoneState
    Nothing -> Empty


-- checks if the move of given 'Point' to given 'Stone' is valid or not
-- TODO : check if the stone is trapped
validMove :: World -> Position -> StoneState -> Bool
validMove w@(World _ _ stones board lastMove ) p@(x,y) side
  | x < 0 || x > 8 || y < 0 || y > 8 = False -- if outside the board
  | seekBoard w p /= Empty = False -- if point is not empty
  | stones == [] = False
  | otherwise = True

-- drop all the stones
-- the stones will be on the nearest cross
releaseMouse :: Coord -> World -> World
releaseMouse (x,y) w =
  if validMove w p side then newWorld else noMove
    where p = ((x-15) `div` 50, (y-15) `div` 50)
          side = getSideFromLastMove (lastMove w)
          b = board w
          newWorld = w { stones = [], board = addPiece b p side , lastMove = StoneMove p side}
          noMove = w { stones = [] }

-- | 'addPiece' adds a element of type t to map
addPiece :: (Map.Map Position s) -> Position -> s -> (Map.Map Position s)
addPiece m point@(px,py) stone = if 0<=px && px<9 && 0<=py && py<9
                           then Map.insert point stone (Map.delete point m)
                           else m

-- removes the stone from given point in the map
removePiece :: (Map.Map Position StoneState) -> Position -> (Map.Map Position StoneState)
removePiece m position = addPiece m position Empty

-- do nothing
idleWorld :: World -> World
idleWorld = id

-- Given the renderer, and the texture and the state of the World,
-- we can render the world. Note that the rendering results in an IO action.
-- This is a wrapper method that clears the rendering target, draws in the window,
-- and swaps the contexts. The actual drawing is done in drawWorld below.
renderWorld :: SDL.Renderer -> [(SDL.Texture, SDL.TextureInfo)] -> (Int, Int) -> World -> IO ()
renderWorld r t d w = do
  SDL.clear r
  drawBackground r (head t) d
  drawBoard r
  drawBowls r (tail t) w
  drawStones r (tail t) w
  drawStickyStone r (tail t) w
  SDL.present r

-- The actual method for drawing that is used by the rendering method above.
drawBackground :: SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> (Int, Int) -> IO ()
drawBackground r (t, ti) (winWidth, winHeight) = do
  -- Get the size of the texture, and we scale it down for a better effect
  -- the original wood.png file is "too big"
  let texHeight = SDL.textureHeight ti `div` 4
  let texWidth = SDL.textureWidth ti `div` 4

  -- Loop and draw the tiled texture
  -- start from left top, draw the texture until it cover the window
  let loop x y
          | y >= winHeight = return ()
          | x >= winWidth = loop 0 (y + fromIntegral texHeight)
          | otherwise = do
              SDL.copy r t Nothing (Just $ C.mkRect (fromIntegral x) (fromIntegral y) texWidth texHeight)
              loop (x + fromIntegral texWidth) y
  loop 0 0

-- Draw squares as a board
drawBoard :: SDL.Renderer -> IO ()
drawBoard r = do
  let loop x y
            | y >= 8 = return ()
            | x >= 8 = loop 0 (y + 1)
            | otherwise = do
                SDL.drawRect r (Just $ C.mkRect (40+x*50) (40+y*50) 51 51)
                loop (x + 1) y
  loop 0 0

-- Draw bowls
drawBowls :: SDL.Renderer -> [(SDL.Texture, SDL.TextureInfo)] -> World -> IO ()
drawBowls r t w =
  let createBowl :: Bowl -> SDL.Rectangle CInt
      createBowl b = C.mkRect (x' - radius) (y' - radius) (2 * radius) (2 * radius)
                      where (x', y') = coordB b
  in do
    SDL.copy r (fst $ head t) Nothing . Just . createBowl $ black $ bowls w
    SDL.copy r (fst $ last t) Nothing . Just . createBowl $ white $ bowls w

-- Draw stones in the map
drawStones :: SDL.Renderer -> [(SDL.Texture, SDL.TextureInfo)] -> World -> IO ()
drawStones r t w =
  let createStone :: (Position, StoneState) -> SDL.Rectangle CInt
      createStone ((x,y), _) =
        C.mkRect (x' - radius) (y' - radius) (2 * radius) (2 * radius)
        where (x',y') = calculatePos (x,y)
  in do
    -- if color is Black
    mapM_ (SDL.copy r (fst $ head t) Nothing . Just . createStone)
      $ filter (\(_, s) -> s == Black) $ Map.toList (board w)
    -- if color is White
    mapM_ (SDL.copy r (fst $ last t) Nothing . Just . createStone)
      $ filter (\(_, s) -> s == White) $ Map.toList (board w)

-- Draw Sticky stone
drawStickyStone :: SDL.Renderer -> [(SDL.Texture, SDL.TextureInfo)] -> World -> IO ()
drawStickyStone r t w =
  let createStone :: Stone -> SDL.Rectangle CInt
      createStone Stone { pos = (px,py), coord = (cx,cy), sticky = sti, captured = _ } =
        if sti
          then C.mkRect (cx - radius) (cy - radius) (2 * radius) (2 * radius)
          else C.mkRect (px' - radius) (py' - radius) (2 * radius) (2 * radius)
          where (px',py') = calculatePos (px,py)
  in do
    -- if color is Black
    mapM_ (SDL.copy r (fst $ head t) Nothing . Just . createStone) $ filter (\s -> color s == True) (stones w)
    -- if color is White
    mapM_ (SDL.copy r (fst $ last t) Nothing . Just . createStone) $ filter (\s -> color s == False) (stones w)


