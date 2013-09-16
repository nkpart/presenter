{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}
module PresenterMain where

import qualified Control.Monad as M
import Debug.Trace
import Data.Monoid 
import Data.Foldable (elem)
import Prelude hiding ((.), id, mapM_, elem)
import Control.Wire hiding (empty)
import qualified Data.Set as Set (insert, delete)
import Data.Set (Set)
import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Image as SDLI
import Graphics.UI.SDL.TTF as SDLF
import SlideTypes
import BFPG

data Slider = Slider {
              _sliderSurface :: Surface
            , _sliderTitleFont :: Font
            , _sliderSheep :: Surface
  }

withSlider :: (Slider -> IO a) -> IO ()
withSlider f = SDL.withInit [SDL.InitEverything] $ do
  SDLF.init
  window_ <- SDL.setVideoMode 800 600 32 [SDL.SWSurface]
  font <- SDLF.openFont "SourceCodePro-Medium.ttf" 28
  sheep <- SDLI.load "sheep.png"
  f $ Slider window_ font sheep
  SDLF.quit


data Nav = GoLeft | GoRight deriving Show

type Zipper a = ([a], a, [a])

mkZipper (a, as) = ([], a, as)
zipperFocus (_, f, _) = f

-- newtype Scene = Scene (forall e m. Wire e m () (SDL.Surface -> IO ()))

navigate :: Nav -> Zipper a -> Zipper a
navigate GoLeft v@([], _, _) = v
navigate GoLeft (l:ls, f, rs) = (ls, l, f:rs)

navigate GoRight v@(_, _, []) = v
navigate GoRight (ls, f, r:rs) = (f:ls, r, rs)

type NEL a = (a, [a])
unsafeNEL (x:xs) = (x, xs)

zipperNavigator :: Monoid e => Zipper a -> Wire e m (Maybe Nav) a
zipperNavigator z = mkState z $ \t (nav, state) -> case nav of
                                                      Just n -> let v = navigate n state in (Right (zipperFocus v), v)
                                                      Nothing -> (Left mempty, state)

help = M.join traceShow

slideWire as@(a, _) = switch (keysDownW >>> keysToNav >>> zipperNavigator (mkZipper as)) a
  where keysToNav = pure (Just GoRight) >>> periodically 1 <|> pure Nothing

main :: IO ()
main = withSlider $ \slider -> do
    go slider bfpg
    return ()
  where
    go slider slides = go_ clockSession $ slideWire $ unsafeNEL $ cycle $ fmap showSlide slides
      where go_ s w = do
                (f, nextWire, session) <- stepSession_ w s Nothing
                f window
                go_ session nextWire
            showSlide = renderSlide sheep font
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

renderSlide :: Monad m => SDL.Surface -> SDLF.Font -> Slide e m a -> (Wire e m a (Surface -> IO ()))
renderSlide sheep font r = 
                case r of 
                  Title xxx color -> pure $ \window -> do
                    paintScreen window $ css2color color
                    SDL.blitSurface sheep Nothing window $ Just $ SDL.Rect 0 0 100 100
                    text <- SDLF.renderTextShaded font xxx (SDL.Color 255 255 255) (SDL.Color 0 0 0)
                    SDL.blitSurface text Nothing window $ Just $ SDL.Rect 100 100 100 100
                    SDL.flip window
                  DisplayWire _ -> pure $ \window -> return ()
                  GenText w -> w >>> arr (\t window -> do
                                              paintScreen window (25, 25, 25)
                                              SDL.blitSurface sheep Nothing window $ Just $ SDL.Rect 0 0 100 100
                                              text <- SDLF.renderTextShaded font t (SDL.Color 255 255 255) (SDL.Color 0 0 0)
                                              SDL.blitSurface text Nothing window $ Just $ SDL.Rect 100 100 100 100
                                              SDL.flip window
                                         )

