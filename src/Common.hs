module Common (
    withSDL
  , withSDLImage
  , withRenderer
  , withWindow
  , mkPoint
  , mkRect
  , rectMoveTo
  , setHintQuality
  , loadTextureWithInfo
) where

import qualified SDL
import qualified SDL.Image

import Control.Monad          (void)
import Control.Monad.IO.Class (MonadIO)
import Data.Text              (Text)

-- State assignment operator
import SDL (($=))

-- Sets SDL library and executes an operation in SDL context.
-- Clears SDL when operation finishes.
withSDL :: (MonadIO m) => m a -> m ()
withSDL op = do
  SDL.initialize []
  void op
  SDL.quit

-- Sets SDL.Image library and executes any given operation in the context.
withSDLImage :: (MonadIO m) => m a -> m ()
withSDLImage op = do
  SDL.Image.initialize []
  void op
  SDL.Image.quit

-- Given title and dimensions it sets SDL Window and uses it to
-- run subsequent operations on the Window. Clears when done.
withWindow :: (MonadIO m) => Text -> (Int, Int) -> (SDL.Window -> m a) -> m ()
withWindow title (x, y) op = do
  w <- SDL.createWindow title p
  SDL.showWindow w
  void $ op w
  SDL.destroyWindow w
  where
    p = SDL.defaultWindow { SDL.windowInitialSize = z }
    z = SDL.V2 (fromIntegral x) (fromIntegral y)

-- Given SDL window, it sets up render and uses it to execute rendering function,
-- clears renderer when done.
withRenderer :: (MonadIO m) => SDL.Window -> (SDL.Renderer -> m a) -> m ()
withRenderer w op = do
  r <- SDL.createRenderer w (-1) rendererConfig
  void $ op r
  SDL.destroyRenderer r

-- Setup renderer config
rendererConfig :: SDL.RendererConfig
rendererConfig = SDL.RendererConfig
  { SDL.rendererType = SDL.SoftwareRenderer -- SDL.AcceleratedVSyncRenderer
  , SDL.rendererTargetTexture = False
  }


-- Sets SDL.HintRenderScaleQuality
setHintQuality :: (MonadIO m) => m ()
setHintQuality = SDL.HintRenderScaleQuality $= SDL.ScaleNearest

-- Loads the texture with info as a tuple.
loadTextureWithInfo :: (MonadIO m) => SDL.Renderer -> FilePath -> m (SDL.Texture, SDL.TextureInfo)
loadTextureWithInfo r p = do
  t <- SDL.Image.loadTexture r p
  i <- SDL.queryTexture t
  pure (t, i)

-- Makes SDL point out of x y
mkPoint :: a -> a -> SDL.Point SDL.V2 a
mkPoint x y = SDL.P (SDL.V2 x y)

-- Makes SDL rectangle out of x y (top left corner) width and height
mkRect :: a -> a -> a -> a-> SDL.Rectangle a
mkRect x y w h = SDL.Rectangle o z
  where
    o = SDL.P (SDL.V2 x y)
    z = SDL.V2 w h

-- Moves SDL.Rectangle to a new position given by tuple (x, y).
-- Rectangle dimensions stay as before.
rectMoveTo :: SDL.Rectangle a -> (a, a) -> SDL.Rectangle a
rectMoveTo (SDL.Rectangle _ d) (x, y) = SDL.Rectangle (mkPoint x y) d

