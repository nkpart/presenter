{-# LANGUAGE NoMonomorphismRestriction #-}
module BFPG where

import Data.Monoid ()
import SlideTypes
import Control.Wire

bfpg = [
       Subtitled "Programming Games in Haskell" "Nick Partridge",
       Title "Two",
       Title "Three",
       Title "Four",
       GenText $ cycleW ["a", "b", "c"],
       GenText $ cycleW ["1", "2", "3"]
       ]

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
