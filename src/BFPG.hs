{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleInstances #-}
module BFPG where

import Utils
import Control.Lens hiding (perform)
import Graphics.UI.SDL.Joystick as SDLJ
import Data.VectorSpace hiding (Sum, getSum)
import Data.Monoid
import SlideTypes
import Control.Wire hiding (window)
import SDLStuff
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as SDLF
import Data.Fixed
import Prelude hiding ((.))
import Text.Printf (printf)
import qualified Control.Monad as M
import qualified Config as C

thrust (Just joystick) = pure f >>> perform
  where f = do
            v <- SDLJ.getButton joystick 11
            return . Accelerate $ if v
              then (0 ::Double, -80 ::Double) + gravity
              else (0, 0) + gravity
        gravity = (0,50)

thrust Nothing = pure (Accelerate 0)

leftMargin = 50

bfpg codeFont tagFont joystick = intro ++ gameLoops ++ introToWires ++ buildingSky ++ movingObjects ++ problems ++ wrapup
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
               , bulleted "Why?" [ "Ocharles is awesome"
                                 , "CampJS is also awesome"
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
                                 , displayFloat time 100
                                 <> ShowCode "time :: Wire e m a Time"
                                 , displayFloat (time >>> derivative_ const 0) 100
                                 <> ShowCode "time >>> derivative :: Wire e m a Time"
                                 , displayFloat timeCycle 100
                                 <> ShowCode "timeFrom 17 >>^ (\\t -> t `mod'` 24)"
                                 , displayFloat timeCycle 100
                                 <> ShowCode "timeCycle = timeFrom 17 >>^ (`mod` 24)"
                                 , displayFloat timeCycle 100
                                 <> displayFloat fractionToNightW 200
                                 <> ShowCode "fractionToNight hour = abs (hour - 12) / 12"
                                 , displayFloat timeCycle 100 
                                 <>  Raw (fractionToNightW >>^ floatString >>^ ("fractionToNight (nf) = " ++) >>^ drawCode' 170)
                                 <>  Raw (pure ("start = " ++ show C.dayColor) >>^ drawCode' 240)
                                 <>  Raw (pure ("end = " ++ show C.nightColor) >>^ drawCode' 310)
                                 <> Raw (fractionToNightW >>^ skyColor >>^ show3 >>^ ("skyColor = " ++) >>^ drawCode' 380)
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
                                 <> Raw (fractionToNightW >>^ skyColor >>> arr (\c window ->
                                        paintRect window c $ Just (SDL.Rect 0 100 800 500)))
                                 <> displayFloat timeCycle 100
                                 <> Raw (fractionToNightW >>^ floatString >>^ ("fractionToNight (nf) = " ++) >>^ drawCode' 170)
                                 <> Raw (pure ("start = " ++ show C.dayColor) >>^ drawCode' 240)
                                 <> Raw (pure ("end = " ++ show C.nightColor) >>^ drawCode' 310)
                                 <> ShowCode "timeCycle >>^ fractionToNight >>^ skyColor"
                                 , Subtitled "Stars" "Don't worry it's only 5 lines of code."
                                 , bulleted "Star System" [
                                                            "Stars are randomly produced"
                                                          , "Stars are gradually removed"
                                                          , "Stars only exist at night time"
                                                          ]
                                 , displayFloat timeCycle 100
                                 <> ShowCode "timeCycle"
                                 , displayFloat timeCycle 100
                                 <> Raw ((hold (0,0) (periodically 0.2 >>> randomStar)) >>^ show >>^ drawCode' 200)
                                 <> ShowCode "timeCycle &&& randomStar"
                                 , titledCode' "Star System" [
                                                        "stars :: Wire e m (Time, Pos) [Pos]"
                                                      , "stars = accum modifier []"
                                                      ,"  where modifier oldStars (h, newStar) "
                                                      ,"          | h > 17.5 = newStar:oldStars"
                                                      ,"          | h < 6 = if null oldStars"
                                                      ,"                      then oldStars"
                                                      ,"                      else tail oldStars"
                                                      ,"          | otherwise = []"
                                                             ]
                                 , Raw ((timeCycle &&& randomStar) >>> stars >>^ (\stars window -> do
                                       drawCode' 200 (show (length stars)) window
                                       M.forM_ stars $ \(x,y) -> paintRect window (lerp C.dayColor (255,255,255) ((fromIntegral (800 - y) / fromIntegral 800) ^ 2)) $ Just $ SDL.Rect x y 2 2
                                       ))
                                 <> displayFloat timeCycle 100
                                 <> ShowCode "(tCycle &&& randomStar) >>> stars >>^ length"
                                 , Title "Full Sky"
                                 <>  Raw (timeCycle >>^ fractionToNight >>^ skyColor >>> arr (\c window ->
                                        paintRect window c $ Just (SDL.Rect 0 100 800 500)))
                                 <>  Raw ((timeCycle &&& randomStar) >>> stars >>^ (\stars window -> do
                                       M.forM_ stars $ \(x,y) -> paintRect window (lerp C.dayColor (255,255,255) ((fromIntegral (800 - y) / fromIntegral 800) ^ 2)) $ Just $ SDL.Rect x y 2 2
                                       ))
                                 <> titledCode' "Full Sky" [ "data Sky = Sky Color [Pos]"
                                                           , "type Pos = (Int, Int)"
                                                           , " "
                                                           , "skyColor :: Wire e m Time Color"
                                                           , "stars :: Wire e m (Time, Pos) [Pos]"
                                                           , " "
                                                           , "skyWire :: Wire e m Time Sky"
                                                           , "skyWire = timeCycle >>>"
                                                           , "  (Sky <$> skyColor"
                                                           , "       <*> (id &&& randomStar) >>> stars)"
                                                           ]
                                 , 
                                   Raw (timeCycle >>^ fractionToNight >>^ skyColor >>> arr (\c window ->
                                        paintRect window c $ Just (SDL.Rect 0 100 800 500)))
                                 <>  Raw ((timeCycle &&& randomStar) >>> stars >>^ (\stars window -> do
                                       M.forM_ stars $ \(x,y) -> paintRect window (lerp C.dayColor (255,255,255) ((fromIntegral (800 - y) / fromIntegral 800) ^ 2)) $ Just $ SDL.Rect x y 2 2
                                       ))
                                 <> bulleted "Full Sky" [ "Small functions"
                                                        , "No shared state"
                                                        , "No effects"
                                                        , "Compositionality - >>>, &&&, >>^, many more"
                                                        , "Pretty RAD"
                                                        ]
                               ]

        show3  = show . map3 floatString
        fractionToNight hour = abs (hour - 12) / 12
        fractionToNightW = timeCycle >>^ fractionToNight
        skyColor nightApproach = C.dayColor + (C.nightColor - C.dayColor) ^* nightApproach
        drawCode' y = \text window -> drawString window (255,255,255) text codeFont (leftMargin, y)
        displayFloat f y = Raw (f >>^ floatString >>^ drawCode' y)
        skyBaseSlide = Raw (pure (\window -> do
               paintRect window C.dayColor $ Just (SDL.Rect 0 100 400 500) 
               drawString window (r3 C.nightColor) (show C.dayColor) codeFont (leftMargin, 150)
               paintRect window C.nightColor $ Just (SDL.Rect 400 100 400 500) 
               drawString window (r3 C.dayColor) (show C.nightColor) codeFont (leftMargin + 400, 150)
               ))
           <> Title "Background"

        movingObjects = let initialObject = object_ (flip const) (ObjectState (leftMargin, 450) (0,0) :: ObjectState (Double, Double)) 
                            moveAndShow accel = Raw $ accel >>> initialObject >>^ renderObject codeFont
                            in
          tagAll "△" [ Subtitled "Objects" "Things that move"
                                   , titledCode' "Object Type" [ "Wire e m (ObjectDiff a) (ObjectState a)"
                                                               , " "
                                                               , "ObjectState { objPosition :: a"
                                                               , "            , objVelocity :: a}"
                                                               , " "
                                                               , "data ObjectDiff a = Accelerate a"
                                                               , "                  | Position a"
                                                               , "                  | Velocity a"
                                                               ]
                                   
                                   , let accel = pure (Accelerate 0, ())
                                      in moveAndShow accel
                                       <> ShowCode "box = object_ (ObjectState (50,450) (0,0))"
                                       <> Title "An Object"
                                   , let accel = pure (Accelerate (5,-5), ())
                                      in moveAndShow accel
                                       <> titledCode' "Moving An Object" [ " "
                                                                         , " "
                                                                         , "accel = pure (Accelerate (5, -5), ())"
                                                                         ]
                                       <> ShowCode "accel >>> box"
                                   , let accel = (for 2 >>> pure (Accelerate 0, ())) 
                                             --> (forI 1 >>> (pure (Accelerate (7500, -7500), ()))) 
                                             --> (pure (Accelerate (0, 50), ()))
                                      in moveAndShow accel
                                       <> titledCode' "Moving An Object" [ " "
                                                                         , " "
                                                                         , "accel = "
                                                                         , "     (for 5 >>> pure (Accelerate 0, ()))"
                                                                         , " --> (forI 1"
                                                                         , "      >>> pure (Acclerate (7500, -7500), ()))"
                                                                         , " --> (pure (Acclerate (0, 50), ()))"
                                                                         ]
                                       <> ShowCode "accel >>> box"
                                   , let accel = thrust joystick >>^ (,())
                                      in moveAndShow accel
                                       <> titledCode' "Moving An Object" [ " "
                                                                         , " "
                                                                         ,"thrust joystick = do"
                                                                         ,"            let gravity = (0, 50)"
                                                                         ,"            v <- SDLJ.getButton joystick 11"
                                                                         ,"            return . Accelerate $ if v"
                                                                         ,"              then (0, -80) + gravity"
                                                                         ,"              else (0, 0) + gravity"
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
                                                             , "       - when you miss an inhibition, everything then fails"
                                                             , "State - wires wrap their own state (at least it is local)"
                                                             , "No Input = polymorphic input"
                                                             , "Effects - could type everything with `m = Identity`"
                                                             ]
                                <> ShowCode "Time -> a -> m (Either e b, Wire e m a b)"
                              , bulleted "Library Installation" [ "SDL + Mac OS X Mavericks + Xcode 5 = D:"
                                                                , "So many yaks - SDL 1.2, SDL_ttf, SDL_image, SDL_..."
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

floatString :: Double -> String
floatString t = printf "%.2f" t

titledCode title code codeFont = Title title
                              <> Raw (pure (\window -> drawLines codeFont (255,255,255) (leftMargin, 150) window code))

drawLines font color (x,y) window lines = do
    forEachI lines $ \(line, position) -> do
      let line' = if null line then " " else line
      (width, height) <- SDLF.textSize font line' 
      drawString window color line' font (x, y + (position * height))

bulleted t s = Bulleted t $ fmap (" • " ++) s

tagAll symbol = fmap (<> Tagged symbol)

stars = accum modifier []
  where modifier oldStars (h, newStar) 
          | h > 17.5 = newStar:oldStars
          | h < 6 = if null oldStars then oldStars else tail oldStars
          | otherwise = []

randomStar :: MonadRandom m => Wire e m a (Int, Int)
randomStar = liftA2 (,) (pure (0,800) >>> noiseRM) (pure (0,600) >>> noiseRM)

timeCycle = timeFrom 17 >>^ (\t -> t `mod'` 24)

renderObject font s@(ObjectState (x, y) _) window = do
    drawString window (255,255,255) (showObj s) font (leftMargin, 150)
    paintRect window (255,255,255) $ Just (SDL.Rect (round x) (round y) 50 50) where showObj (ObjectState pos vel) = "Pos: " ++ floatString2 pos ++ ", Vel: " ++ floatString2 vel 

floatString2 = show . over both floatString
