Presenter - a Haskell/SDL/Netwire slide presentation app!
======

This is the application I built to run a presentation I made to the [BFPG](http://www.bfpg.org) on 24 September, 2013.

The presentation was mainly on the [Netwire](http://hackage.haskell.org/package/netwire) library, and this app is a live demonstration of various Wires.

The slides are in `src/BFPG.hs`. Theoretically, you can just run `cabal install --only-dependencies && cabal build`, then `./run` and it will work. 


Dependencies
---

I used:

  * GHC 7.6.3
  * SDL 1.2.x, with SDL_ttf and SDL_image - you will need these packages installed before installing the haskell libaries
