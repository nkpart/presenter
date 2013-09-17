{-# LANGUAGE ExistentialQuantification #-}
module SlideTypes where

import Control.Wire
import Data.Monoid

data Slide e m a = Title String
           | Subtitled String String
           | GenText (Wire e m a String)
           | forall b. (Show e, Show a, Show b) => DisplayWire (Wire e IO a b)
           | Layered (Slide e m a) (Slide e m a)
           | Blank

instance Monoid (Slide e m a) where
  mempty = Blank
  mappend = Layered

