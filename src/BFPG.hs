{-# LANGUAGE NoMonomorphismRestriction #-}
module BFPG where

import Data.Monoid
import SlideTypes
import Control.Wire
import SDLStuff
import qualified Graphics.UI.SDL as SDL
import Data.Fixed
import Prelude hiding ((.))
import Text.Printf (printf)

timeScale = 4.0

bfpg codeFont = [
         Subtitled "Programming Games in Haskell" "Nick Partridge"
       , Title "DEMO"
       , Title "Three"
       , Title "EXAMPLES"
       -- TODO: hold "" vs pure ""
       , GenText (fmap (:[]) $ hold ' ' (periodically 0.5 >>> cycleW "bfpg"))
         <> ShowCode "periodically 0.5 >>> cycleW \"bfpg\""
       , GenText (fmap (:[]) $ hold ' ' (cycleW "bfpg" >>> periodically 0.5)) -- example of preceding operator vs proceding
         <> ShowCode "cycleW \"bfpg\" >>> periodically 0.5"
       , Raw (fastTime >>> arr (\t window -> drawString window (255,255,255) (floatString t) codeFont (50, 100)))
         <> ShowCode "time"
         -- this is not 17 because fastTime multiples by 6
       , Raw (fastTimeFrom (17 / timeScale) >>^ (\t -> t `mod'` 24) >>> arr (\t window -> drawString window (255,255,255) (floatString t) codeFont (50, 100)))
         <> ShowCode "timeFrom 17 >>^ (\\t -> t `mod'` 24))"
       ]

fastTime = time >>^ (* timeScale)
fastTimeFrom x = timeFrom x >>^ (* timeScale)

floatString t = printf "%.2f" t

-- data Wire' a b = Wire' (f :: Time -> a -> (Either Error b, Wire' a b) )
-- data GameState = GameState Int
-- data Events

-- gameLoopWire :: Wire Events GameState
-- gameLoopWire = mkState (GameState 0) f
--   where f time (events, state) = let nextState = gameUpdate time events state
--                                   in (Right nextState, nextState)
-- 
-- fff theWire theSession = do
--   events <- pollEvents
--   (gameState, newWire, newSession) <- stepSession theWire theSession events
--   render gameState
--   fff newWire newSession
-- 
-- main = do
--     session <- clockSession
--     fff gameLoopWire session


-- gameUpdate :: Time -> Events -> GameState -> GameState

-- {- 
-- $state = ...
-- 
-- while true do
--    inputs = getEvents
--    updateWorld(inputs)
--    render()
-- end
-- 
-- - }
