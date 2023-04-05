module Lib
    ( mainApp
    ) where

import qualified SDL
import qualified Common as C

import Data.Foldable          (foldl')
import Foreign.C.Types

type Coord = (CInt, CInt)

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
  { pos :: Coord
  , sticky :: Bool -- if the mouse is having the stone
  , captured :: Coord -- distance from the mouse to the stone
  }

-- bowl of the stones which we take the stones from
data Bowls = Bowls
  { black :: Stone
  , white :: Stone
  }

-- The world
data World = World
  { exiting :: Bool
  , bowls :: Bowls
  , stones :: [Stone]
  }

initialWorld :: World
initialWorld = World
  { exiting = False
  , bowls = Bowls { black = Stone { pos = (530, 65), sticky = False, captured = (0,0) }
                  , white = Stone { pos = (530, 115), sticky = False, captured = (0,0) }
                  }
  , stones = [ Stone { pos = (40, 40), sticky = False, captured = (0,0)}
              , Stone { pos = (40, 90), sticky = False, captured = (0,0)}
              ]
  }

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

-- takes the position of the mouse, update the status of a stone if the mouse is on the stone
pressMouse :: Coord -> World -> World
pressMouse (x, y) w =
  let updateStones :: [Stone] -> [Stone]
      updateStones [] = []
      updateStones (s:ss)
        -- if the mouse is on a stone, update the stone status
        | distance < radius' = s { sticky = True, captured = (x-sx, y-sy) } : ss
        | otherwise = s : updateStones ss
        where (sx, sy) = pos s
              distance = sqrt (fromIntegral $ (x-sx)^(2::Int) + (y-sy)^(2::Int))
              radius' = fromIntegral radius::Double
  in w { stones = updateStones $ stones w }

-- drop all the stones
-- TODO: make sure each stone is on a cross
releaseMouse :: Coord -> World -> World
releaseMouse _ w = w { stones = map (\s -> s { sticky = False}) (stones w) }

-- takes the position of the mouse,
-- updates the stone if is sticky
moveObject :: Coord -> World -> World
moveObject (x,y) w =
  let updatePos :: [Stone] -> [Stone]
      updatePos [] = []
      updatePos (s:ss) = if sticky s
                            then s { pos = (x - fst (captured s), y - snd (captured s)) } : updatePos ss
                            else s : updatePos ss
  in w { stones = updatePos $ stones w }

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
  drawStones r (last t) w
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
  let createStone :: Stone -> SDL.Rectangle CInt
      createStone s = C.mkRect (x' - radius) (y' - radius) (2 * radius) (2 * radius)
                      where (x', y') = pos s
  in do
    SDL.copy r (fst $ head t) Nothing . Just . createStone $ black $ bowls w
    SDL.copy r (fst $ last t) Nothing . Just . createStone $ white $ bowls w

-- Draw stones
drawStones :: SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> World -> IO ()
drawStones r (t, _) w =
  let createStone :: Stone -> SDL.Rectangle CInt
      createStone c = C.mkRect (x' - r') (y' - r') (2 * r') (2 * r')
                      where r' = radius; (x', y') = pos c
  in mapM_ (SDL.copy r t Nothing . Just . createStone) (stones w)

