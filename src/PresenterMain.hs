{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
-- {-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module PresenterMain where

import Data.Maybe (listToMaybe)
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
import DataStructures
import System.Directory

data Theme = Theme { 
           _titleFont :: Font, 
           _subtitleFont :: Font, 
           _codeFont :: Font, 
           _backgroundColor :: String 
           }

macFont f s = do
    let prefixes = ["/Library/Fonts/", "/Users/nkpart/Library/Fonts/"]
        paths = fmap (++ f) prefixes
    toOpen <- head <$> M.filterM doesFileExist paths
    SDLF.openFont toOpen s

withSlider :: (Theme -> Surface -> IO a) -> IO ()
withSlider f = SDL.withInit [SDL.InitEverything] $ do
  SDLF.init
  window_ <- SDL.setVideoMode 800 600 32 [SDL.SWSurface]
  theme <- Theme <$> macFont "Comic Sans MS Bold.ttf" 40 
                 <*> macFont "Comic Sans MS.ttf" 28 
                 <*> macFont "SourceCodePro-Medium.ttf" 28 
                 <*> pure "#252525"
  -- sheep <- SDLI.load "sheep.png"
  f theme window_ -- $ Slider window_ font sheep
  SDLF.quit

zipperNavigator :: Monoid e => Zipper a -> Wire e m (Maybe Nav) a
zipperNavigator z = mkState z $ \t (nav, state) -> case nav of
                                                      Just n -> let v = navigate n state in (Right (zipperFocus v), v)
                                                      Nothing -> (Left mempty, state)

help = M.join traceShow

slideWire as@(a, _) = switch (keyReleasesW >>> keysToNav' >>> zipperNavigator (mkZipper as)) a
  where keysToNav = pure (Just GoRight) >>> periodically 3 <|> pure Nothing
        keysToNav' = arr (listToMaybe . concatMap (\v -> case v of
                                              SDL.SDLK_h -> [GoLeft]
                                              SDL.SDLK_LEFT -> [GoLeft]
                                              SDL.SDLK_l -> [GoRight]
                                              SDL.SDLK_RIGHT -> [GoRight]
                                              _ -> []
                                              ))

main :: IO ()
main = withSlider $ \theme window -> do
    go theme window $ bfpg (_codeFont theme)
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

keyReleasesW = mkFixM $ \t a -> do
  ks <- keyReleases []
  return $ Right ks

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

keyReleases :: [SDL.SDLKey] -> IO [SDL.SDLKey]
keyReleases ks = do
  ev <- SDL.pollEvent
  case ev of
    SDL.NoEvent -> return ks
    SDL.KeyDown k | SDL.symKey k == SDL.SDLK_q && elem SDL.KeyModLeftMeta (SDL.symModifiers k) -> error "Done"
    SDL.KeyUp k -> keyReleases (SDL.symKey k:ks) -- (Set.insert (SDL.symKey k) keysDown)
    SDL.Quit -> error "Done"
    _ -> keyReleases ks

renderSlide :: Monad m => SDL.Surface -> Theme -> Slide e m a -> Wire e m a (Surface -> IO ())
renderSlide sheep (Theme {..}) slide = fmap (\f window -> layFoundation window >> f window >> SDL.flip window) $ xxx slide
  where xxx slide = case slide of 
                      Layered bottom top -> proc a -> do
                        f <- xxx bottom -< a
                        g <- xxx top -< a
                        returnA -< (\window -> f window >> g window)
                      Title title -> pure $ \window -> do
                        SDL.blitSurface sheep Nothing window $ Just $ SDL.Rect 0 0 100 100
                        text <- SDLF.renderTextBlended _titleFont title (SDL.Color 255 255 255)
                        SDL.blitSurface text Nothing window $ Just $ SDL.Rect 100 100 100 100
                        return ()
                      Subtitled main lesser -> pure $ \window -> do
                        text <- SDLF.renderTextBlended _titleFont main (SDL.Color 255 255 255)
                        SDL.blitSurface text Nothing window $ Just $ SDL.Rect 100 100 100 100
                        text2 <- SDLF.renderTextBlended _subtitleFont lesser (SDL.Color 255 255 255)
                        SDL.blitSurface text2 Nothing window $ Just $ SDL.Rect 100 200 100 100
                        return ()
                      -- DisplayWire _ -> pure $ \window -> return ()
                      ShowCode code -> (pure $ \window -> do
                        renderedCode <- SDLF.renderTextBlended _codeFont code (SDL.Color 255 255 255)
                        paintRect window (0, 0, 0) $ Just $ SDL.Rect 0 500 800 100
                        SDL.blitSurface renderedCode Nothing window $ Just $ SDL.Rect 50 530 100 100
                        return ())
                      Raw f -> f
                      Blank -> pure $ \window -> return ()
                      GenText w -> w >>> arr (\t window -> do
                                                  SDL.blitSurface sheep Nothing window $ Just $ SDL.Rect 0 0 100 100
                                                  text <- SDLF.renderTextBlended _titleFont t (SDL.Color 255 255 255)
                                                  SDL.blitSurface text Nothing window $ Just $ SDL.Rect 100 100 100 100
                                                  return ()
                                             )
        layFoundation window = paintScreen window $ css2color $ _backgroundColor
