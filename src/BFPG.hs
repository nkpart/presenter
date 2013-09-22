{-# LANGUAGE NoMonomorphismRestriction #-}
module BFPG where

import Data.VectorSpace hiding (Sum, getSum)
import Data.Monoid
import SlideTypes
import Control.Wire
import SDLStuff
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as SDLF
import Data.Fixed
import Prelude hiding ((.))
import Text.Printf (printf)
import Data.List (intercalate)
import qualified Control.Monad as M
import qualified Config as C

leftMargin = 50

bfpg codeFont tagFont = buildingSky ++ intro ++ gameLoops ++ introToWires ++ buildingSky ++ movingObjects ++ problems
  where intro = [
                 Subtitled "Programming Games in Haskell" "Nick Partridge - @nkpart - nkpart@gmail.com"
               , bulleted "Bio" [ "Not a games programmer"
                                , "Written exactly 2 apps with this technology"
                                , "Have no idea what FRP is"
                                ]
               , bulleted "The Technology" [ "Haskell"
                                           , "SDL - inputs. drawing rectangles, images and text"
                                           , "Netwire - FRP(*). Values/functions that change over time"
                                           ]
               , bulleted "Why?" [ "Ocharles is super rad"
                                 , "CampJS is also super rad"
                                 , "DEMO"]
               , Bulleted "Topics" [ "Functional Game Core"
                                   , "Intro to Wires"
                                   , "Building the Sky"
                                   , "An Object"
                                   , "Problems/Gotchas"
                                   ]
                <> Raw (pure $ \window ->
                       forEachI "⟳→★△☹" $ \(t,idx) -> drawString window (255,255,255) (t:[]) tagFont (500, 130 + (idx * 70)))
               ]

        titledCode' a b = titledCode a b codeFont
        gameLoops = tagAll "⟳" [ bulleted "Functional Game Core" [
                                    "Other Cores"
                                  , "Imperative Blub vs Imperative Haskell"
                                  , "Contrived Flaw/Convenient Segue"
                                  ]

                               , titledCode' "Functional Cores" [
                                                                  "webApp :: Request -> Response"
                                                                , " "
                                                                , "databaseQuery :: Query -> ResultSet"
                                                                , " "
                                                                , "databaseUpdate :: "
                                                                , "    Update -> Database -> Database"
                                                                ]
                               , titledCode' "Functional Game Cores" [
                                                                  "game :: "
                                                                , "    Events -> GameState -> GameState"
                                                                ]
                               , titledCode' "Imperatively" [
                                                             "var gameState = InitGameState();"
                                                           , "while (!gameState.isQuit()) {"
                                                           , "  var events = PollForEvents();"
                                                           , "  UpdateGameState(gameState, events);"
                                                           , "  RenderGameState(gameState);"
                                                           , "}"
                                                           ]
                               , titledCode' "Haskell Game Loop" [
                                                                   "play = loop initialGameState"
                                                                 , "loop current = do"
                                                                 , "  events <- pollEvents"
                                                                 , "  let next = updateGameState current events"
                                                                 , "  renderGameState next"
                                                                 , "  loop next"
                                                                 ]
                               , Subtitled "Contrived Flaw" "No Time."
                               , bulleted "Changes over Time" [ "Physics"
                                                              , "Rate-limited actions"
                                                              , "Environmental effects"
                                                              , "AI?"
                                                              , "Recovery/damage over time"
                                                              , "Most of the Fun Stuff"
                                                              ]
                               ]

        introToWires = tagAll "→" [ bulleted "Intro to Wires" [ "netwire", "Control.Wire" ]
                                  , bulleted "Wires Produce Values" ["a - input", "b - output"]
                                  <> ShowCode "a -> b"
                                  , bulleted "Values over Time" ["Time - delta", "a - input", "b - output"]
                                  <> ShowCode "Time -> a -> b"
                                  , bulleted "Not every instant" ["Time - delta", "a - input", "b - output", "e - inhibits with"]
                                  <> ShowCode "Time -> a -> Either e b"
                                  , bulleted "Change *themselves* over time" ["Time - delta", "a - input", "b - output", "e - inhibits with"]
                                  <> ShowCode "Time -> a -> (Either e b, Wire e a b)"
                                  , bulleted "May have effects" ["Time - delta", "a - input", "b - output", "e - inhibits with", "m - effect of 'stepping' the wire"]
                                  <> ShowCode "Time -> a -> m (Either e b, Wire e m a b)"
                                  ]

        buildingSky = tagAll "★" [ Title "Building Sky"
                                 , bulleted "Sky Components" [
                                                               "Time of Day"
                                                             , "Background color"
                                                             , "Stars"]
                                 , Raw (fastTime >>> arr (\t window -> drawString window (255,255,255) (floatString t) codeFont (leftMargin, 100)))
                                 <> ShowCode "time :: Wire e m a Time"
                                 , Raw ((fastTime >>> derivative_ const 0) >>> arr (\t window -> drawString window (255,255,255) (floatString t) codeFont (leftMargin, 100)))
                                 <> ShowCode "time >>> derivative_ const 0 :: Wire e m a Time"
                                 , Raw (timeCycle >>^ floatString >>^ drawCode' (leftMargin, 100))
                                 <> ShowCode "timeFrom 17 >>^ (\\t -> t `mod'` 24)"
                                 , Raw (timeCycle >>^ floatString >>^ drawCode' (leftMargin, 100))
                                 <> ShowCode "timeCycle = timeFrom 17 >>^ (`mod` 24)"
                                 , Raw (timeCycle >>^ floatString >>^ drawCode' (leftMargin, 100))
                                 <>  Raw (timeCycle >>^ fractionToNight >>^ floatString >>^ drawCode' (leftMargin, 200))
                                 <> ShowCode "fractionToNight hour = abs (hour - 12) / 12"
                                 , Raw (timeCycle >>^ floatString >>^ drawCode' (leftMargin, 100))
                                 <>  Raw (timeCycle >>^ fractionToNight >>^ floatString >>^ drawCode' (leftMargin, 200))
                                 <> Raw (timeCycle >>^ fractionToNight >>^ skyColor >>^ show3 >>^ drawCode' (leftMargin, 300)) --drawColor' (SDL.Rect 100 0 100 100))
                                 <> ShowCode "skyColor nf = start + (end - start) ^* nf"
                                 , titledCode' "Background Components" ["timeCycle = timeFrom 17 >>^ (`mod` 24)"
                                                   ," "
                                                   ,"fractionToNight hour = abs (hour - 12) / 12"
                                                   ," "
                                                   ,"skyColor nf = start + (end - start) ^* nf"
                                                   ," "
                                                   ," "
                                                   ,"timeCycle >>^ fractionToNight >>^ skyColor"
                                                   ]
                                 , Title "Background"
                                 <>  Raw (timeCycle >>^ fractionToNight >>^ skyColor >>> arr (\c window ->
                                        paintRect window c $ Just (SDL.Rect 0 100 800 500)))
                                 <> Raw (timeCycle >>^ floatString >>^ drawCode' (leftMargin, 100))
                                 <> Raw (timeCycle >>^ fractionToNight >>^ floatString >>^ drawCode' (leftMargin, 200))
                                 <> Raw (timeCycle >>^ fractionToNight >>^ skyColor >>^ show3 >>^ drawCode' (leftMargin, 300)) --drawColor' (SDL.Rect 100 0 100 100))
                                 <> ShowCode "timeCycle >>^ fractionToNight >>^ skyColor"
                                 ]
        show3 (a,b,c) = "(" ++ floatString a <> ", " <> floatString b <> ", " <> floatString c <> ")"
        fractionToNight hour = abs (hour - 12) / 12
        skyColor nightApproach = C.dayColor + (C.nightColor - C.dayColor) ^* nightApproach
        drawCode' pos = \text window -> drawString window (255,255,255) text codeFont pos
        drawColor' rect = \color window -> paintRect window color $ Just rect 
        timeCycle = fastTimeFrom 17 >>^ (\t -> t `mod'` 24)
        skyBaseSlide = Raw (pure (\window -> do
               paintRect window C.dayColor $ Just (SDL.Rect 0 100 400 500) 
               drawString window (r3 C.nightColor) (show C.dayColor) codeFont (leftMargin, 150)
               paintRect window C.nightColor $ Just (SDL.Rect 400 100 400 500) 
               drawString window (r3 C.dayColor) (show C.nightColor) codeFont (leftMargin + 400, 150)
               ))
         <> Title "Background"

        movingObjects = tagAll "△" [ Title "Moving Objects"
                                   ]

        problems = tagAll "☹" [ Title "Problems/Gotchas"
                              , Title "This" <> GenText (fmap (:[]) $ hold ' ' (periodically 0.5 >>> cycleW "bfpg"))
                                <> ShowCode "periodically 0.5 >>> cycleW \"bfpg\""
                              , Title "That" <> GenText (fmap (:[]) $ hold ' ' (cycleW "bfpg" >>> periodically 0.5)) -- example of preceding operator vs proceding
                                <> ShowCode "cycleW \"bfpg\" >>> periodically 0.5"
                              , bulleted "Too many features" [
                                                               "Errors - difficult to track where they are introduced"
                                                             , "       - when you miss an inhibition, every then fails"
                                                             , "State - wires wrap their own state (at least it is local)"
                                                             , "No Input = polymorphic input"
                                                             , "Effects - could type everything with `m = Identity`"
                                                             ]
                                
                                <> ShowCode "Time -> a -> m (Either e b, Wire e m a b)"
                              ]
       -- -- TODO: hold "" vs pure ""
       -- , Raw (fastTimeFrom (17 / timeScale) >>^ (\t -> t `mod'` 24) >>> arr (\t window -> drawString window (255,255,255) (floatString t) codeFont (50, 100)))
       --   <> ShowCode "timeFrom 17 >>^ (\\t -> t `mod'` 24))"

fastTime = time >>^ (* timeScale)
fastTimeFrom x = timeFrom (x / timeScale) >>^ (* timeScale)

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
--
--
--

titledCode title code codeFont = 
    Title title
 <> Raw (arr (\t window -> drawLines codeFont (255,255,255) (leftMargin, 150) window code))

drawLines font color (x,y) window lines = do
    forEachI lines $ \(line, position) -> do
      let line' = if null line then " " else line
      (width, height) <- SDLF.textSize font line' 
      drawString window color line' font (x, y + (position * height))

forEachI xs f = M.forM_ (zip xs [0..]) f


timeScale = 3.0

bulleted t s = Bulleted t $ fmap (" • " ++) s

tagAll symbol = fmap (<> Tagged symbol)

r3 (a,b,c) = (round a, round b, round c)
