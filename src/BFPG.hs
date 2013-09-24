{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleInstances #-}
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

bfpg codeFont tagFont = intro ++ gameLoops ++ introToWires ++ buildingSky ++ movingObjects ++ problems ++ wrapup
  where intro = [
                 Subtitled "Using Netwire for Games in Haskell" "Nick Partridge - @nkpart - nkpart@gmail.com"
               , bulleted "Bio" [ "Not a games programmer"
                                , "Written exactly 2 apps with this technology"
                                , "Have no idea what FRP is"
                                ]
               , bulleted "The Technology" [ "Making Games with Haskell"
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
                                                                 , "→ let next = updateGameState current events"
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

        introToWires = tagAll "→" [ bulleted "Intro to Wires" [ "netwire package", "Control.Wire module" ]
                                  <> ShowCode "Wire e m a b"
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
                                  , titledCode' "Netwire Game Loop" [
                                                                   "play = loop gameWire clockSession"
                                                                 , "loop current session = do"
                                                                 , "  events <- pollEvents"
                                                                 , "  (next, nextWire, nextSession)"
                                                                 , "      <- stepSession current session events"
                                                                 , "  renderGameState next"
                                                                 , "  loop nextWire nextSession"
                                                                 ]
                                  <> ShowCode "gameWire :: Wire e IO Events GameState"
                                  ]

        buildingSky = tagAll "★" [ Title "Building Sky"
                                 , bulleted "Sky Components" [
                                                               "Time of Day"
                                                             , "Background color"
                                                             , "Stars"]
                                 , Raw (fastTime >>> arr (\t window -> drawString window (255,255,255) (floatString t) codeFont (leftMargin, 100)))
                                 <> ShowCode "time :: Wire e m a Time"
                                 , Raw ((fastTime >>> derivative_ const 0) >>> arr (\t window -> drawString window (255,255,255) (floatString t) codeFont (leftMargin, 100)))
                                 <> ShowCode "time >>> derivative :: Wire e m a Time"
                                 , Raw (timeCycle >>^ floatString >>^ drawCode' (leftMargin, 100))
                                 <> ShowCode "timeFrom 17 >>^ (\\t -> t `mod'` 24)"
                                 , Raw (timeCycle >>^ floatString >>^ drawCode' (leftMargin, 100))
                                 <> ShowCode "timeCycle = timeFrom 17 >>^ (`mod` 24)"
                                 , Raw (timeCycle >>^ floatString >>^ drawCode' (leftMargin, 100))
                                 <>  Raw (timeCycle >>^ fractionToNight >>^ floatString >>^ drawCode' (leftMargin, 200))
                                 <> ShowCode "fractionToNight hour = abs (hour - 12) / 12"
                                 , Raw (timeCycle >>^ floatString >>^ drawCode' (leftMargin, 100))
                                 <>  Raw (timeCycle >>^ fractionToNight >>^ floatString >>^ ("fractionToNight (nf) = " ++) >>^ drawCode' (leftMargin, 170))
                                 <>  Raw (pure ("start = " ++ show C.dayColor) >>^ drawCode' (leftMargin, 240))
                                 <>  Raw (pure ("end = " ++ show C.nightColor) >>^ drawCode' (leftMargin, 310))
                                 <> Raw (timeCycle >>^ fractionToNight >>^ skyColor >>^ show3 >>^ ("skyColor = " ++) >>^ drawCode' (leftMargin, 380))
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
                                 , skyBaseSlide <> Title "Background"
                                 , Title "Background"
                                 <>  Raw (timeCycle >>^ fractionToNight >>^ skyColor >>> arr (\c window ->
                                        paintRect window c $ Just (SDL.Rect 0 100 800 500)))
                                 <> Raw (timeCycle >>^ floatString >>^ drawCode' (leftMargin, 100))
                                 <> Raw (timeCycle >>^ fractionToNight >>^ floatString >>^ drawCode' (leftMargin, 200))
                                 <> Raw (timeCycle >>^ fractionToNight >>^ skyColor >>^ show3 >>^ drawCode' (leftMargin, 300)) --drawColor' (SDL.Rect 100 0 100 100))
                                 <> ShowCode "timeCycle >>^ fractionToNight >>^ skyColor"
                                 , Subtitled "Stars" "Don't worry it's only 5 lines of code."
                                 , bulleted "Star System" [
                                                            "Stars are randomly produced"
                                                          , "Stars are gradually removed"
                                                          , "Stars only exist at night time"
                                                          ]
                                 , Raw (timeCycle >>^ floatString >>^ drawCode' (leftMargin, 100))
                                 , Raw (((timeCycle >>^ floatString >>^ drawCode' (leftMargin, 100))
                                  &&& ((hold (0,0) (periodically 0.2 >>> randomStar)) >>^ show >>^ drawCode' (leftMargin, 200))) >>^ (\(a,b) w -> a w >> b w))
                                 <> ShowCode "timeCycle &&& randomStar"
                                 , titledCode' "Star System" [
                                                      "stars = accum modifier []"
                                                      ,"  where modifier oldStars (h, newStar) "
                                                      ,"          | h > 17.5 = newStar:oldStars"
                                                      ,"          | h < 6 = if null oldStars"
                                                      ,"                      then oldStars"
                                                      ,"                      else tail oldStars"
                                                      ,"          | otherwise = []"
                                                             ]
                                 , Raw ((timeCycle &&& randomStar) >>> stars >>^ (\stars window -> do
                                       drawCode' (leftMargin, 200) (show (length stars)) window
                                       M.forM_ stars $ \(x,y) -> paintRect window (lerp C.dayColor (255,255,255) ((fromIntegral (800 - y) / fromIntegral 800) ^ 2)) $ Just $ SDL.Rect x y 2 2
                                       ))
                                 <> Raw (timeCycle >>^ floatString >>^ drawCode' (leftMargin, 100))
                                 <> ShowCode "(timeCycle &&& randomStar) >>> stars >>^ length"
                                 , Title "Full Sky"
                                 <>  Raw (timeCycle >>^ fractionToNight >>^ skyColor >>> arr (\c window ->
                                        paintRect window c $ Just (SDL.Rect 0 100 800 500)))
                                 <>  Raw ((timeCycle &&& randomStar) >>> stars >>^ (\stars window -> do
                                       M.forM_ stars $ \(x,y) -> paintRect window (lerp C.dayColor (255,255,255) ((fromIntegral (800 - y) / fromIntegral 800) ^ 2)) $ Just $ SDL.Rect x y 2 2
                                       ))
                                 , 
                                   Raw (timeCycle >>^ fractionToNight >>^ skyColor >>> arr (\c window ->
                                        paintRect window c $ Just (SDL.Rect 0 100 800 500)))
                                 <>  Raw ((timeCycle &&& randomStar) >>> stars >>^ (\stars window -> do
                                       M.forM_ stars $ \(x,y) -> paintRect window (lerp C.dayColor (255,255,255) ((fromIntegral (800 - y) / fromIntegral 800) ^ 2)) $ Just $ SDL.Rect x y 2 2
                                       ))
                                 <> bulleted "Full Sky" [ "Small functions"
                                                        , "No shared state"
                                                        , "No effects"
                                                        , "Compositionality"
                                                        , "Pretty RAD"
                                                        ]
                               ]

        show3 (a,b,c) = "(" ++ floatString a <> ", " <> floatString b <> ", " <> floatString c <> ")"
        fractionToNight hour = abs (hour - 12) / 12
        skyColor nightApproach = C.dayColor + (C.nightColor - C.dayColor) ^* nightApproach
        drawCode' pos = \text window -> drawString window (255,255,255) text codeFont pos
        drawColor' rect = \color window -> paintRect window color $ Just rect 
        skyBaseSlide = Raw (pure (\window -> do
               paintRect window C.dayColor $ Just (SDL.Rect 0 100 400 500) 
               drawString window (r3 C.nightColor) (show C.dayColor) codeFont (leftMargin, 150)
               paintRect window C.nightColor $ Just (SDL.Rect 400 100 400 500) 
               drawString window (r3 C.dayColor) (show C.nightColor) codeFont (leftMargin + 400, 150)
               ))
         <> Title "Background"

        movingObjects = tagAll "△" [ bulleted "Objects" [ "Physics!"
                                                               , "Integrals/Differentiation"
                                                               , "Movement equations"
                                                               ]
                                   , titledCode' "Object Type" [ "Wire e m (ObjectDiff a) (ObjectState a)"
                                                               , " "
                                                               , "ObjectState { objPosition :: a"
                                                               , "            , objVelocity :: a}"
                                                               , " "
                                                               , "data ObjectDiff a = Accelerate a"
                                                               , "                  | Position a"
                                                               , "                  | Velocity a"
                                                               ]
                                   
                                   , let x = 1
                                      in Raw (pure (Accelerate 0, ())
                                       >>> object_ (flip const) (ObjectState (leftMargin, 450) (0,0) :: ObjectState (Double, Double)) 
                                       >>^ renderObject True codeFont)
                                       <> ShowCode "box = object_ (ObjectState (50,450) (0,0))"
                                       <> Title "An Object"
                                   , let accel = (for 2 >>> pure (Accelerate 0, ())) --> (forI 1 >>> (pure (Accelerate (7500, -7500), ()))) --> (pure (Accelerate (0, 50), ()))
                                      in Raw (accel
                                       >>> object_ (flip const) (ObjectState (leftMargin, 450) (0,0) :: ObjectState (Double, Double)) 
                                       >>^ renderObject True codeFont)
                                       <> titledCode' "Moving An Object" [ " "
                                                                         , " "
                                                                         , "accel = "
                                                                         , "     (for 5 >>> pure (Accelerate 0, ()))"
                                                                         , " --> (forI 1"
                                                                         , "      >>> pure (Acclerate (7500, -7500), ()))"
                                                                         , " --> (pure (Acclerate (0, 50), ()))"
                                                                         ]
                                       <> ShowCode "accel >>> box"
                                   ]

        problems = tagAll "☹" [ bulleted "Problems/Gotchas" [ "Composition order"
                                                            , "Wire features"
                                                            , "Library Installation - (not Netwire)"
                                                            ]
                              , Title "This?" <> GenText (fmap (:[]) $ hold ' ' (periodically 0.5 >>> cycleW "BFPG"))
                                <> ShowCode "periodically 0.5 >>> cycleW \"BFPG\""
                              , Title "or this?" <> GenText (fmap (:[]) $ hold ' ' (cycleW "BFPG" >>> periodically 0.5)) -- example of preceding operator vs proceding
                                <> ShowCode "cycleW \"BFPG\" >>> periodically 0.5"
                              , bulleted "Too many features" [
                                                               "Errors - difficult to track where they are introduced"
                                                             , "       - when you miss an inhibition, every then fails"
                                                             , "State - wires wrap their own state (at least it is local)"
                                                             , "No Input = polymorphic input"
                                                             , "Effects - could type everything with `m = Identity`"
                                                             ]
                                <> ShowCode "Time -> a -> m (Either e b, Wire e m a b)"
                              , bulleted "Library Installation" [ "SDL + Mac OS X Mavericks + Xcode 5 = D:"
                                                                , "So many yaks"
                                                                , ""
                                                                ]
                              ]
        wrapup = [ Title "Wrapping Up"
                 , bulleted "Other Resources" [ "Ocharles' Tutorials"
                                        , "Netwire Hackage Docs"
                                        , "Any SDL tutorial" 
                                        ]
                 , bulleted "Ideas for the Future!" [ "Fork and clean up my code"
                                                    , "Hack night"
                                                    ]
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

timeScale = 1.0

bulleted t s = Bulleted t $ fmap (" • " ++) s

tagAll symbol = fmap (<> Tagged symbol)

r3 (a,b,c) = (round a, round b, round c)

stars = accum modifier []
  where modifier oldStars (h, newStar) 
          | h > 17.5 = newStar:oldStars
          | h < 6 = if null oldStars then oldStars else tail oldStars
          | otherwise = []

randomStar :: MonadRandom m => Wire e m a (Int, Int)
randomStar = liftA2 (,) (pure (0,800) >>> noiseRM) (pure (0,600) >>> noiseRM)

timeCycle = fastTimeFrom 17 >>^ (\t -> t `mod'` 24)

renderObject t font s@(ObjectState (x, y) vel) window = do
    M.when t $ drawString window (255,255,255) (showObj s) font (leftMargin, 150)
    paintRect window (255,255,255) $ Just (SDL.Rect (round x) (round y) 50 50) where showObj (ObjectState pos vel) = "Pos: " ++ floatString2 pos ++ ", Vel: " ++ floatString2 vel 

floatString2 (a,b) = "(" ++ floatString a ++ ", " ++ floatString b ++ ")"
