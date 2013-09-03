{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ExistentialQuantification #-}

module PresenterMain where

-- import Control.Lens hiding (within, perform)
-- import Data.VectorSpace hiding (Sum, getSum)
import Data.Monoid 
import Data.Foldable (elem)
import Prelude hiding ((.), id, mapM_, elem)
import Control.Wire hiding (empty)
import qualified Data.Set as Set (insert, delete)
import Data.Set (Set)
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as SDLI
import Graphics.UI.SDL.TTF as SDLF

data Slider = Slider {
              _sliderSurface :: Surface
            , _sliderTitleFont :: Font
            , _sliderSheep :: Surface
  }

withSlider f = SDL.withInit [SDL.InitEverything] $ do
  SDLF.init
  window <- SDL.setVideoMode 800 600 32 [SDL.SWSurface]
  font <- SDLF.openFont "SourceCodePro-Medium.ttf" 28
  sheep <- SDLI.load "sheep.png"
  f $ Slider window font sheep
  SDLF.quit

data Slide = Title String
           | forall a b e. (Show e, Show a, Show b) => DisplayWire (Wire e IO a b)

data Nav = GoLeft | GoRight

type Zipper a = ([a], a, [a])

navigate :: Nav -> Zipper a -> Zipper a
navigate GoLeft v@([], _, _) = v
navigate GoLeft (l:ls, f, rs) = (ls, l, f:rs)

navigate GoRight v@(_, _, []) = v
navigate GoRight (ls, f, r:rs) = (f:ls, r, rs)

slidePresenterW :: (Slide, [Slide]) -> Wire e m (Maybe Nav) Slide
slidePresenterW = undefined

renderSlide slide surface titleFont = do
  return ()

main :: IO ()
main = withSlider $ \slider -> do
    go slider $ [Title "Hi There 2", Title "Next"]
    return ()
  where
    go slider slides = go_ clockSession $ pure $ head slides
      where go_ s w = do
                (r, w, s) <- stepSession_ w s ()
                case r of 
                  Title xxx -> do
                    SDL.blitSurface sheep Nothing window $ Just $ SDL.Rect 0 0 100 100
                    text <- SDLF.renderTextShaded font xxx (SDL.Color 255 255 255) (SDL.Color 0 0 0)
                    SDL.blitSurface text Nothing window $ Just $ SDL.Rect 100 100 100 100
                    SDL.flip window
                  DisplayWire _ -> return ()
                go_ s w
            window = _sliderSurface slider
            sheep = _sliderSheep slider
            font = _sliderTitleFont slider

keysDownW = mkStateM mempty $ \_ (_, keys) -> do
  newKeys <- parseEvents keys
  return (Right newKeys, newKeys)

parseEvents :: Set SDL.SDLKey -> IO (Set SDL.SDLKey)
parseEvents keysDown = do
  ev <- SDL.pollEvent
  case ev of
    SDL.NoEvent -> return keysDown
    SDL.KeyDown k | SDL.symKey k == SDL.SDLK_q && elem SDL.KeyModLeftMeta (SDL.symModifiers k) -> error "Done"
    SDL.KeyDown k -> parseEvents (Set.insert (SDL.symKey k) keysDown)
    SDL.KeyUp k -> parseEvents (Set.delete (SDL.symKey k) keysDown)
    SDL.Quit -> error "Done"
    _ -> parseEvents keysDown

