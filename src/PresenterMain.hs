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

-- data Loop = TapToStart
          -- | Play
          -- | ShowVictory GameState Sky

-- data SDLRenderer = SDLRenderer {
                    -- _screen :: SDL.Surface,
                    -- _sheep :: SDL.Surface
                  -- }

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

main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
    window <- SDL.setVideoMode 800 600 32 [SDL.SWSurface]
    sheep <- SDLI.load "sheep.png"
    loop window sheep
    return ()
  where
    loop window sheep = do
      SDL.blitSurface sheep Nothing window $ Just $ SDL.Rect 0 0 100 100
      SDL.flip window
      loop window sheep

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

