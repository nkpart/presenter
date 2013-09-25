module DataStructures where

data Nav = GoLeft | GoRight deriving Show

type Zipper a = ([a], a, [a])

mkZipper (a, as) = ([], a, as)

zipperFocus (_, f, _) = f

-- newtype Scene = Scene (forall e m. Wire e m () (SDL.Surface -> IO ()))

navigate :: Nav -> Zipper a -> Zipper a
navigate GoLeft v@([], _, _) = v
navigate GoLeft (l:ls, f, rs) = (ls, l, f:rs)

navigate GoRight v@(_, _, []) = v
navigate GoRight (ls, f, r:rs) = (f:ls, r, rs)

type NEL a = (a, [a])
unsafeNEL (x:xs) = (x, xs)

