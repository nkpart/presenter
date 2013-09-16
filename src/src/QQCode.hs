module QQCode where

import Language.Haskell.TH.Quote
import Language.Haskell.Exts.Parser

-- qqCode :: QuasiQuoter
-- qqCode = QuasiQuoter f undefined undefined undefined
--   where f s = case parseExp s of
--                 ParseOk a -> return a
--                 _ -> fail $ "Couldn't parse " ++ s

