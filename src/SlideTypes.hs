{-# LANGUAGE ExistentialQuantification #-}
module SlideTypes where

import Control.Wire

data Slide e m a = Title String String
           | GenText (Wire e m a String)
           | forall b. (Show e, Show a, Show b) => DisplayWire (Wire e IO a b)
