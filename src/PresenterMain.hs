{-# LANGUAGE Arrows #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE Rank2Types #-}

module PresenterMain where

import Utils
import qualified Control.Concurrent
import Graphics.UI.SDL.Joystick as SDLJ
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Control.Monad as M
import Data.Monoid
import Data.Foldable (elem)
import Prelude hiding ((.), id, mapM_, elem)
import Control.Wire hiding (empty, window)
import Graphics.UI.SDL as SDL
-- import Graphics.UI.SDL.Image as SDLI
import Graphics.UI.SDL.TTF as SDLF
import SlideTypes
import SDLStuff
import BFPG
import DataStructures
import System.Directory
import Resources (resourcePath)

data Theme = Theme { 
           _titleFont :: Font, 
           _subtitleFont :: Font, 
           _codeFont :: Font, 
           _tagFont :: Font,
           _backgroundColor :: String 
           }

macFont f s = do
    let prefixes = ["/System/Library/Fonts/"
                   , "/Library/Fonts/"
                   , "/Users/nkpart/Library/Fonts/"
                   , "/Library/Fonts/Microsoft/"
                   ]
        paths = resourcePath f : fmap (++ f) []
    toOpen <- head <$> M.filterM doesFileExist paths
    SDLF.openFont toOpen s

withSlider :: (Theme -> Surface -> IO a) -> IO ()
withSlider f = SDL.withInit [SDL.InitEverything, SDL.InitJoystick] $ do
  SDLF.init
  -- SDLJ.open 0
  window_ <- SDL.setVideoMode 800 600 32 [SDL.SWSurface]
  theme <- Theme <$> macFont "SofiaProLight.ttf" 40 
                 <*> macFont "SofiaProLight.ttf" 28 
                 <*> macFont "SourceCodePro-Medium.ttf" 28 
                 <*> macFont "Apple Symbols.ttf" 72 
                 <*> pure "#252525"
  f theme window_
  SDLF.quit

zipperNavigator :: Monoid e => Zipper a -> Wire e m (Maybe Nav) a
zipperNavigator z = mkState z $ \_ (nav, state) -> case nav of
                                                      Just n -> let v = navigate n state in (Right (zipperFocus v), v)
                                                      Nothing -> (Left mempty, state)

slideWire as@(a, _) = switch (allEvents >>> eventsToNav >>> zipperNavigator (mkZipper as)) a
  where keysToNav = pure (Just GoRight) >>> periodically 3 <|> pure Nothing
        keysToNav' = arr (listToMaybe . concatMap (\v -> case v of
                                              SDL.SDLK_h -> [GoLeft]
                                              SDL.SDLK_LEFT -> [GoLeft]
                                              SDL.SDLK_l -> [GoRight]
                                              SDL.SDLK_RIGHT -> [GoRight]
                                              _ -> []
                                              ))

goalDtime = 1 / 30.0

main :: IO ()
main = withSlider $ \theme window -> do
    -- print =<< SDLJ.name 0
    -- j <- SDLJ.open 0
    go theme window $ bfpg (_codeFont theme) (_tagFont theme) Nothing -- (Just j)
    return ()
  where
    go theme window slides = go_ clockSession ((slideWire . unsafeNEL . cycle . fmap showSlide $ slides) &&& dtime )
      where go_ s w = do
                -- SDLJ.update
                -- joyIsOpen <- SDLJ.opened 0
                -- M.unless joyIsOpen (M.void $ SDLJ.open 0)
                ((f, dt), nextWire, session) <- stepSession_ w s Nothing
                f window
                M.when (dt < goalDtime) $ do
                  Control.Concurrent.threadDelay (round ((goalDtime - dt) / 1000))
                go_ session nextWire
            showSlide = renderSlide window theme

eventsToNav = arr $ listToMaybe . mapMaybe f
  where f (SDL.KeyUp k) = checkKey $ SDL.symKey k
        f (SDL.JoyButtonUp k v) = checkJoy k v
        f _ = Nothing
        checkKey k = case k of
            SDL.SDLK_h -> Just GoLeft
            SDL.SDLK_LEFT -> Just GoLeft
            SDL.SDLK_l -> Just GoRight
            SDL.SDLK_RIGHT -> Just GoRight
            _ -> Nothing
        checkJoy _ v | v == 9 || v == 14 = Just GoRight 
        checkJoy _ v | v == 8 || v == 13 = Just GoLeft
        checkJoy _ _ = Nothing

allEvents = arr (\_ -> poll []) >>> perform
    where poll ks = do
            ev <- SDL.pollEvent
            -- M.unless (ev == SDL.NoEvent) $ print ev
            case ev of
              SDL.NoEvent -> return ks
              SDL.KeyDown k | SDL.symKey k == SDL.SDLK_q && elem SDL.KeyModLeftMeta (SDL.symModifiers k) -> error "Done"
              SDL.Quit -> error "done"
              v -> poll (v:ks)

renderSlide :: Monad m => SDL.Surface -> Theme -> Slide e m a -> Wire e m a (Surface -> IO ())
renderSlide sheep (Theme {..}) slide = fmap (\f window -> layFoundation window >> f window >> SDL.flip window) $ xxx slide
  where xxx slide = case slide of 
                      Layered bottom top -> proc a -> do
                        f <- xxx bottom -< a
                        g <- xxx top -< a
                        returnA -< (\window -> f window >> g window)
                      Title title -> pure $ \window -> do
                        SDL.blitSurface sheep Nothing window $ Just $ SDL.Rect 0 0 100 100
                        text <- SDLF.renderUTF8Blended _titleFont title (SDL.Color 255 255 255)
                        SDL.blitSurface text Nothing window $ Just $ SDL.Rect 50 50 100 100
                        return ()
                      Subtitled main lesser -> pure $ \window -> do
                        text <- SDLF.renderUTF8Blended _titleFont main (SDL.Color 255 255 255)
                        SDL.blitSurface text Nothing window $ Just $ SDL.Rect 50 50 100 100
                        text2 <- SDLF.renderUTF8Blended _subtitleFont lesser (SDL.Color 255 255 255)
                        SDL.blitSurface text2 Nothing window $ Just $ SDL.Rect 50 150 100 100
                        return ()
                      Bulleted main points -> pure $ \window -> do
                        text <- SDLF.renderUTF8Blended _titleFont main (SDL.Color 255 255 255)
                        SDL.blitSurface text Nothing window $ Just $ SDL.Rect 50 50 100 100
                        forEachI points $ \(text, index) -> do
                          text2 <- SDLF.renderUTF8Blended _subtitleFont text (SDL.Color 255 255 255)
                          SDL.blitSurface text2 Nothing window $ Just $ SDL.Rect 50 (150 + index * 70) 100 100
                        return ()
                      -- DisplayWire _ -> pure $ \window -> return ()
                      ShowCode code -> pure $ \window -> do
                        renderedCode <- SDLF.renderTextBlended _codeFont code (SDL.Color 255 255 255)
                        paintRect window (0, 0, 0) $ Just $ SDL.Rect 0 500 800 100
                        SDL.blitSurface renderedCode Nothing window $ Just $ SDL.Rect 50 530 100 100
                        return ()
                      Tagged symbol -> pure $ \window -> do
                        SDL.blitSurface sheep Nothing window $ Just $ SDL.Rect 0 0 100 100
                        text <- SDLF.renderUTF8Blended _tagFont symbol (SDL.Color 255 255 255)
                        SDL.blitSurface text Nothing window $ Just $ SDL.Rect 720 30 100 100
                        return ()
                      Raw f -> f
                      Blank -> pure (pure (pure ()))
                      GenText w -> w >>^ (\t window -> do
                                                SDL.blitSurface sheep Nothing window $ Just $ SDL.Rect 0 0 100 100
                                                text <- SDLF.renderTextBlended _titleFont t (SDL.Color 255 255 255)
                                                SDL.blitSurface text Nothing window $ Just $ SDL.Rect 100 100 100 100
                                                return ())
        layFoundation window = paintScreen window $ css2color _backgroundColor
