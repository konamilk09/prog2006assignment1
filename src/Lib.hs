module Lib
    ( mainApp
    ) where

import qualified SDL
import qualified Common as C

import qualified Data.Map as Map
import Data.List (nub)
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
  , scoreBlack :: Int
  , scoreWhite :: Int
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
  , scoreBlack = 0
  , scoreWhite = 0
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
      newStone (World _ bowl _ _ lm _ _)
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
seekBoard (World _ _ _ m _ _ _) p = case Map.lookup p m of
    Just stoneState -> stoneState
    Nothing -> Empty


-- checks if the move of given 'Point' to given 'Stone' is valid or not
-- TODO : check if the stone is trapped
validMove :: World -> Position -> StoneState -> Bool
validMove w@(World _ _ stone _ _ _ _) p@(x,y) st
  | x < 0 || x > 8 || y < 0 || y > 8 = False -- if outside the board
  | seekBoard w p /= Empty = False -- if point is not empty
  | stone == [] = False -- if the mouse was not clicked on the correct bowl
  | not $ ifTrapped vWorld p st = True -- if the point is not trapped
  -- If point is trapped then check if on placing the stone whether opposites stone sare captured or not
  | (seekBoard w up == os) && (ifTrapped vWorld up os) = True
  | (seekBoard w down == os) && (ifTrapped vWorld down os) = True
  | (seekBoard w left == os) && (ifTrapped vWorld left os) = True
  | (seekBoard w right == os) && (ifTrapped vWorld right os) = True
  | otherwise = False
  where vWorld = vMove w p st
        os = getOppositeStone st
        up = (x,y+1)
        down = (x,y-1)
        right = (x+1,y)
        left = (x-1,y)

-- assume the world after playing the move
vMove :: World -> Position -> StoneState -> World
vMove (World _ _ _ b _ _ _) point st = World {
    exiting = False
  , bowls = Bowls { black = Bowl { coordB = (530, 65), stickyB = False }
                  , white = Bowl { coordB = (530, 115), stickyB = False }
                  }
  , stones = []
  , board = addPiece b point st
  , lastMove = StoneMove point st
  , scoreBlack = 0
  , scoreWhite = 0
}

-- if the list from findTrappedGroup has Nothing, the group is not trapped
ifTrapped :: World -> Position -> StoneState -> Bool
ifTrapped w p st = not $ elem Nothing $ findTrappedGroup w p st []

-- finds trapped group of stones in the world
-- starting from the given point and going in all directions, returns if the group that
-- the starting point belong to is trapped.
findTrappedGroup :: World -> Position -> StoneState -> [Maybe Position] -> [Maybe Position]
findTrappedGroup w p@(x,y) st seenPoints
    | x < 0 || x > 8 || y < 0 || y > 8  = seenPoints -- if the point is out of bound
    | elem (pure p) seenPoints = seenPoints -- if already checked the point
    | seekBoard w p == Empty = Nothing:seenPoints -- if the point is Empty->this group is not trapped
    | seekBoard w p == Ko = Nothing:seenPoints -- if the point is Ko
    | seekBoard w p /= st = seenPoints -- if the point has different type of stone
    | otherwise = findTrappedGroup w left st
        $ findTrappedGroup w right st
        $ findTrappedGroup w up st
        $ findTrappedGroup w down st ((pure p):seenPoints)
    where up = (x,y+1)
          down = (x,y-1)
          right = (x+1,y)
          left = (x-1,y)

-- updates the score of the stone
updateScore :: World -> Int -> StoneState -> World
updateScore w@(World _ _ _ _ _ sb sw) score st
    | st == Black = w { scoreBlack = sb + score }
    | otherwise = w { scoreWhite = sw + score }

-- adds 'Ko' to the game at given point
addKo :: World -> (Maybe Position) -> World
addKo w@(World _ _ _ m _ _ _) (Just p) = w { board = (addPiece m p Ko) }
addKo w Nothing = w

-- removes group from the point
removeGroups :: World -> Position -> StoneState -> World
removeGroups w (x,y) st
    -- If number of points captured is one then check if Ko is formed and add Ko to the board if neccesary
    | len == 1 && isKo = updateScore (addKo (removeStones w pointsToBeRemoved) (pointsToBeRemoved !! 0)) 1 os
    -- TODO : updateScore
    | otherwise =  updateScore (removeStones w pointsToBeRemoved) len os
    where (up, down, left, right) = ((x,y+1), (x,y-1), (x-1,y), (x+1,y))
          removeUp = removablePoints up st w
          removeDown = removablePoints down st w
          removeLeft = removablePoints left st w
          removeRight = removablePoints right st w
          pointsToBeRemoved = nub (removeUp ++ removeDown ++ removeLeft ++ removeRight)
          len = length pointsToBeRemoved
          os = getOppositeStone st
          -- check ko
          (px, py) = if length removeUp == 1
            then up
            else if length removeDown == 1
                 then down
                 else if length removeLeft == 1
                      then left else right
          (up', down', left', right') = ((px,py+1), (px,py-1), (px-1,py), (px+1,py))
          isKo = length ((removablePoints up' os w) ++ (removablePoints down' os w) ++ (removablePoints left' os w) ++ (removablePoints right' os w)) == 1

-- returns a list of points in the group if the group is trapped
removablePoints :: Position -> StoneState -> World -> [Maybe Position]
removablePoints p st w
  | ifTrapped w p st = findTrappedGroup w p st []
  | otherwise = []

-- removes the stone from given point in the map
removePiece :: (Map.Map Position StoneState) -> Position -> (Map.Map Position StoneState)
removePiece m position = addPiece m position Empty

-- given a list of points which are candidate of being removed
-- remove the stones if it is dead group
removeStones :: World -> [Maybe Position] -> World
removeStones w [] = w
removeStones w (Nothing:_) = w
removeStones (World e bowl stone m st sb sw) ((Just p):xs) =
  removeStones (World e bowl stone (removePiece m p) st sb sw) xs

-- adds a stone to the point
-- After playing the move, removes the captured points of the opposite stone if present
playMove :: World -> Position -> StoneState -> World
playMove w@(World _ _ _ b _ _ _) p st =
  removeGroups w {stones = [], board = addPiece b p st, lastMove = StoneMove p st} p os
  where os = getOppositeStone st

-- drop all the stones
-- the stones will be on the nearest cross
releaseMouse :: Coord -> World -> World
releaseMouse (x,y) w =
  if validMove w p side then newWorld else noMove
    where p = ((x-15) `div` 50, (y-15) `div` 50)
          side = getSideFromLastMove (lastMove w)
          newWorld = playMove w p side
          noMove = w { stones = [] }

-- | 'addPiece' adds a element of type t to map
addPiece :: (Map.Map Position s) -> Position -> s -> (Map.Map Position s)
addPiece m point@(px,py) stone = if 0<=px && px<9 && 0<=py && py<9
                           then Map.insert point stone (Map.delete point m)
                           else m

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


