{-# LANGUAGE NoMonomorphismRestriction #-}
module SDLStuff where

import Graphics.UI.SDL as SDL
-- import Graphics.UI.SDL.Image as SDLI
import Graphics.UI.SDL.TTF as SDLF
import qualified Control.Monad as M

-- SDL Helpers
paintWithRGB screen r g b z = (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen r g b >>= z

paintScreen screen (r, g, b) = paintWithRGB screen (round r) (round g) (round b) $ SDL.fillRect screen Nothing

paintRect screen (r, g, b) rect = M.void $ paintWithRGB screen (round r) (round g) (round b) $ SDL.fillRect screen rect -- (Just $ toSDLRect height rect)

drawString screen (r, g, b) string font (x, y) = do
                                                  text <- SDLF.renderUTF8Blended font string (SDL.Color r g b)
                                                  SDL.blitSurface text Nothing screen $ Just $ SDL.Rect x y 100 100
                                                  return ()

-- CSS color -> Color
-- "#AABBCC"

css2color :: String -> (Float, Float, Float)
css2color rgb | length rgb == 6 = let (r, gb) = splitAt 2 rgb
                                      (b, g) = splitAt 2 gb
                                   in (code2Hex r, code2Hex g, code2Hex b)
css2color ('#':rgb)             = css2color rgb
css2color _                     = error "Bad CSS color"

code2Hex c = read ("0x" ++ c) :: Float
