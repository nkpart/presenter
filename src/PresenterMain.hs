-- {-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# LANGUAGE ImplicitParams #-}
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
import SDLStuff
import BFPG

data Slider = Slider {
              _sliderSurface :: Surface
            , _sliderTitleFont :: Font
            , _sliderSheep :: Surface
  }

data Theme = Theme {
           _titleFont :: Font,
           _subtitleFont :: Font,
           _backgroundColor :: String
}

withSlider :: (Theme -> Surface -> IO a) -> IO ()
withSlider f = SDL.withInit [SDL.InitEverything] $ do
  SDLF.init
  window_ <- SDL.setVideoMode 800 600 32 [SDL.SWSurface]
  theme <- Theme <$> SDLF.openFont "SourceCodePro-Bold.ttf" 40 <*> SDLF.openFont "SourceCodePro-Medium.ttf" 28 <*> pure "#252525"
  -- sheep <- SDLI.load "sheep.png"
  f theme window_ -- $ Slider window_ font sheep
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
main = withSlider $ \theme window -> do
    go theme window bfpg
    return ()
  where
    go theme window slides = go_ clockSession (slideWire . unsafeNEL . cycle . fmap showSlide $ slides)
      where go_ s w = do
                (f, nextWire, session) <- stepSession_ w s Nothing
                f window
                go_ session nextWire
            showSlide = renderSlide window theme

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

layFoundation window (Theme {..}) = paintScreen window $ css2color _backgroundColor

renderSlide :: Monad m => SDL.Surface -> Theme -> Slide e m a -> Wire e m a (Surface -> IO ())
renderSlide sheep theme@(Theme {..}) r = fmap (\f window -> layFoundation window theme >> f window >> SDL.flip window) $ 
                case r of 
                  Title title -> pure $ \window -> do
                    SDL.blitSurface sheep Nothing window $ Just $ SDL.Rect 0 0 100 100
                    text <- SDLF.renderTextBlended _titleFont title (SDL.Color 255 0 255)
                    SDL.blitSurface text Nothing window $ Just $ SDL.Rect 100 100 100 100
                    return ()
                  Subtitled main lesser -> pure $ \window -> do
                    text <- SDLF.renderTextBlended _titleFont main (SDL.Color 255 0 255)
                    SDL.blitSurface text Nothing window $ Just $ SDL.Rect 100 100 100 100
                    text2 <- SDLF.renderTextBlended _subtitleFont lesser (SDL.Color 255 0 255)
                    SDL.blitSurface text2 Nothing window $ Just $ SDL.Rect 100 200 100 100
                    return ()
                  DisplayWire _ -> pure $ \window -> return ()
                  GenText w -> w >>> arr (\t window -> do
                                              SDL.blitSurface sheep Nothing window $ Just $ SDL.Rect 0 0 100 100
                                              text <- SDLF.renderTextBlended _titleFont t (SDL.Color 255 255 255)
                                              SDL.blitSurface text Nothing window $ Just $ SDL.Rect 100 100 100 100
                                              return ()
                                         )

