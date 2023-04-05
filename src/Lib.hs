module Lib
    ( mainApp
    ) where

import qualified SDL
import qualified Common as C

import Data.Foldable          (foldl')

-- Intents
data Intent
  = Idle
  | Quit

-- The world
newtype World = World {exiting :: Bool}

initialWorld :: World
initialWorld = World
  { exiting = False
  }

-- Main entry to our application logic. It takes the handle to the SDL Window,
-- sets everything up and executes the main application loop: handle user inputs,
-- and draw the world.
mainApp :: SDL.Window -> IO ()
mainApp win =
    C.withRenderer win $ \r -> do
      t <- C.loadTextureWithInfo r "./assets/wood.png"

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
      SDL.destroyTexture (fst t)

-- Given a list of events, update the world
updateWorld :: World -> [SDL.Event] -> World
updateWorld w
  = foldl' (flip applyIntent) w
  . fmap (payloadToIntent . SDL.eventPayload)


-- Convert the SDL event to Intent
payloadToIntent :: SDL.EventPayload -> Intent
payloadToIntent SDL.QuitEvent            = Quit -- window CLOSE pressed
payloadToIntent (SDL.KeyboardEvent e)    = -- When Q is pressed, quit also
  if SDL.keysymKeycode (SDL.keyboardEventKeysym e) == SDL.KeycodeQ then Quit else Idle
payloadToIntent _                        = Idle



-- Notice the point-free notation - we have defined our function without referencing the last argument World
applyIntent :: Intent -> World -> World
applyIntent Idle        = idleWorld
applyIntent Quit        = quitWorld

-- do nothing
idleWorld :: World -> World
idleWorld = id

-- let's exit
quitWorld :: World -> World
quitWorld w = w { exiting = True }

-- Given the renderer, and the texture and the state of the World,
-- we can render the world. Note that the rendering results in an IO action.
-- This is a wrapper method that clears the rendering target, draws in the window,
-- and swaps the contexts. The actual drawing is done in drawWorld below.
renderWorld :: SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> (Int, Int) -> World -> IO ()
renderWorld r t d _ = do
  SDL.clear r
  drawBackground r t d
  drawSquares r
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
drawSquares :: SDL.Renderer -> IO ()
drawSquares r = do
  let loop x y
            | y >= 8 = return ()
            | x >= 8 = loop 0 (y + 1)
            | otherwise = do
                SDL.drawRect r (Just $ C.mkRect (20+x*50) (20+y*50) 51 51)
                loop (x + 1) y

  loop 0 0

