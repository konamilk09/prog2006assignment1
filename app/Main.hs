{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified Common as C
import qualified Lib

main :: IO ()
main = C.withSDL $ C.withSDLImage $ do
  C.setHintQuality
  C.withWindow "Go game" (640, 480) $ Lib.mainApp