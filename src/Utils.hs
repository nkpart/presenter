{-# LANGUAGE NoMonomorphismRestriction #-}
module Utils where

import qualified Control.Monad as M

r3 = map3 round

map3 f (a,b,c) = (f a, f b, f c)

forEachI xs f = M.forM_ (zip xs [0..]) f
