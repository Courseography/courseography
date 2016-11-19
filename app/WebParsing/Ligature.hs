{- | Utilities for expanding ligatures in unicode text.

     Currently this module expands the following ligatures:ﬀ,ﬁ,ﬂ,ﬃ,ﬄ,ﬅ,ﬆ

     From https://hackage.haskell.org/package/ligature.
-}
module WebParsing.Ligature where

import Prelude hiding (concatMap)

import Data.Text

-- | If a character is a ligature, expand it to several characters
expandLigature :: Char -> Text
expandLigature '\xFB00' = cons 'f' (cons 'f' empty)
expandLigature '\xFB01' = cons 'f' (cons 'i' empty)
expandLigature '\xFB02' = cons 'f' (cons 'l' empty)
expandLigature '\xFB03' = cons 'f' (cons 'f' (cons 'i' empty))
expandLigature '\xFB04' = cons 'f' (cons 'f' (cons 'l' empty))
expandLigature '\xFB05' = cons 'f' (cons 't' empty)
expandLigature '\xFB06' = cons 's' (cons 't' empty)
expandLigature c = singleton c

-- | Expand all ligatures in the text
expand :: Text -> Text
expand txt = concatMap expandLigature txt
