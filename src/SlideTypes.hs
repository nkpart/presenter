{-# LANGUAGE ExistentialQuantification #-}
module SlideTypes where

import Control.Wire
import Data.Monoid
import qualified Graphics.UI.SDL as SDL

data Slide e m a = Title String
           | Subtitled String String
           | Bulleted String [String]
           | GenText (Wire e m a String)
           | ShowCode String
           | Tagged String
           | Raw (Wire e m a (SDL.Surface -> IO ()))
           | Layered (Slide e m a) (Slide e m a)
           | Blank

instance Monoid (Slide e m a) where
  mempty = Blank
  mappend = Layered

