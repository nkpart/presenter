{-# LANGUAGE ForeignFunctionInterface #-}
module MainWrapper where
import PresenterMain (main)
foreign export ccall "haskell_main" main :: IO ()
