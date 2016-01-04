-- | This is another hand-written lexer, this time for the Xtract
--   command-language.  The entry point is lexXtract.  You don't
--   normally need to use this module directly - the lexer is called
--   automatically by the parser.  (We only expose this interface
--   for debugging purposes.)
--
--   The Xtract command language is very like the XPath specification.

module Text.XML.HaXml.Xtract.Lex
  ( lexXtract
  , Posn(..)
  , TokenT(..)
  , Token
  ) where

import Data.Char


type Token = Either String (Posn, TokenT)

data Posn = Pn Int		-- char index only
        deriving Eq

instance Show Posn where
      showsPrec _p (Pn c) = showString "char pos " . shows c

data TokenT =
      Symbol String
    | TokString String		--     begins with letter
    | TokNum Integer		--     begins with digit
    deriving Eq

instance Show TokenT where
    showsPrec _p (Symbol s) = showString s
    showsPrec _p (TokString s) = showString s
    showsPrec _p (TokNum n) = shows n

emit :: TokenT -> Posn -> Token
emit tok p = forcep p `seq` Right (p,tok)
  where forcep (Pn n) = n

lexerror :: String -> Posn -> [Token]
lexerror s p = [Left ("Lexical error in selection pattern at "++show p++": "
                       ++s++"\n")]

addcol :: Int -> Posn -> Posn
addcol n (Pn c) = Pn (c+n)

newline, tab :: Posn -> Posn
newline (Pn c) = Pn (c+1)
tab     (Pn c) = Pn (((c`div`8)+1)*8)

white :: Char -> Posn -> Posn
white '\t' = tab
white ' '  = addcol 1
white '\n' = addcol 1
white '\r' = addcol 1
white '\xa0' = addcol 1

blank :: (Posn->String->[Token]) -> Posn-> String-> [Token]
blank _ _ []       = []
blank k p (' ': s) = blank k (addcol 1 p) s
blank k p ('\t':s) = blank k (tab p) s
blank k p ('\n':s) = blank k (newline p) s
blank k p ('\r':s) = blank k p s
blank k p ('\xa0': s) = blank k (addcol 1 p) s
blank k p    s     = k p s

----
-- | First argument is a transformer for pattern strings, e.g. map toLower,
--   but only applying to parts of the pattern not in quotation marks.
--   (Needed to canonicalise HTML where tags are case-insensitive, but
--   attribute values are case sensitive.)
lexXtract :: (String->String) -> String -> [Token]
lexXtract f = selAny f (Pn 1)

syms :: [Char]
syms = "/[]()@,=*&|~$+-<>"

selAny :: (String->String) -> Posn -> String -> [Token]
selAny _ _ [] = []
selAny f p ('/':'/':ss) = emit (Symbol "//") p:  selAny f (addcol 2 p) ss
selAny f p ('!':'=':ss) = emit (Symbol "!=") p:  selAny f (addcol 2 p) ss
selAny f p ('<':'=':ss) = emit (Symbol "<=") p:  selAny f (addcol 2 p) ss
selAny f p ('>':'=':ss) = emit (Symbol ">=") p:  selAny f (addcol 2 p) ss
selAny f p ('\'':ss)    = emit (Symbol "'") p:
                          accumulateUntil '\'' (Symbol "'") [] p (addcol 1 p) ss
                                          (selAny f)
selAny f p ('"':ss)     = emit (Symbol "\"") p:
                          accumulateUntil '"' (Symbol "\"") [] p (addcol 1 p) ss
                                          (selAny f)
selAny f p ('_':ss)     = gatherName f "_" p (addcol 1 p) ss (blank (selAny f))
selAny f p (':':ss)     = gatherName f ":" p (addcol 1 p) ss (blank (selAny f))
selAny f p ('.':'=':'.':ss) = emit (Symbol ".=.") p:  selAny f (addcol 3 p) ss
selAny f p ('.':'!':'=':'.':ss)
                            = emit (Symbol ".!=.") p: selAny f (addcol 4 p) ss
selAny f p ('.':'<':'.':ss) = emit (Symbol ".<.") p:  selAny f (addcol 3 p) ss
selAny f p ('.':'<':'=':'.':ss)
                            = emit (Symbol ".<=.") p: selAny f (addcol 4 p) ss
selAny f p ('.':'>':'.':ss) = emit (Symbol ".>.") p:  selAny f (addcol 3 p) ss
selAny f p ('.':'>':'=':'.':ss)
                            = emit (Symbol ".>=.") p: selAny f (addcol 4 p) ss
selAny f p ('.':'/':ss)     = emit (Symbol "./") p:  selAny f (addcol 2 p) ss
selAny f p (s:ss)
    | s `elem` syms   = emit (Symbol [s]) p:     selAny f (addcol 1 p) ss
    | isSpace s       = blank (selAny f) p (s:ss)
    | isAlpha s       = gatherName f [s] p (addcol 1 p) ss (blank (selAny f))
    | isDigit s       = gatherNum    [s] p (addcol 1 p) ss (blank (selAny f))
    | otherwise       = lexerror "unrecognised pattern" p

gatherName :: (String->String) -> String -> Posn -> Posn -> String
              -> (Posn->String->[Token]) -> [Token]
gatherName f acc pos p (s:ss) k
  | isAlphaNum s || s `elem` "-_:" = gatherName f (s:acc) pos (addcol 1 p) ss k
gatherName f acc pos p ss k =
  emit (TokString (f (reverse acc))) pos: k p ss

gatherNum :: String -> Posn -> Posn -> String
             -> (Posn->String->[Token]) -> [Token]
gatherNum acc pos p (s:ss) k
  | isHexDigit s = gatherNum (s:acc) pos (addcol 1 p) ss k
gatherNum acc pos p ss k =
  emit (TokNum (read (reverse acc))) pos: k p ss

accumulateUntil :: Char -> TokenT -> String -> Posn -> Posn -> String
                   -> (Posn->String->[Token]) -> [Token]
accumulateUntil c _tok _acc pos  p  [] _k =
    lexerror ("found end of pattern while looking for "++c
              :" to match opening quote at "++show pos) p
accumulateUntil c  tok  acc pos  p (s:ss) k
    | c==s       = emit (TokString (reverse acc)) pos:
                                  emit tok p: k (addcol 1 p) ss
    | isSpace s  = accumulateUntil c tok (s:acc) pos (white s p) ss k
    | otherwise  = accumulateUntil c tok (s:acc) pos (addcol 1 p) ss k
