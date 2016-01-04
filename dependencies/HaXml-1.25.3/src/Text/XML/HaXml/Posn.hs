-- | Define a position datatype for giving locations in error messages.
module Text.XML.HaXml.Posn
  (
  -- * Position type
    Posn()
  -- ** Constructors of a new position
  , posInNewCxt    -- :: String -> Maybe Posn -> Posn
  , noPos          -- :: Posn
  -- ** Strictifier
  , forcep
  -- ** Modifiers
  , addcol, newline, tab, white
  -- ** Accessors
  , posnFilename, posnLine, posnColumn
  ) where

import Data.Char

-- | Source positions contain a filename, line, column, and an
--   inclusion point, which is itself another source position,
--   recursively.
data Posn = Pn String !Int !Int (Maybe Posn)
        deriving (Eq)

posnFilename :: Posn -> FilePath
posnFilename (Pn f _ _ _) = f

posnLine, posnColumn :: Posn -> Int
posnLine   (Pn _ x _ _) = x
posnColumn (Pn _ _ x _) = x

-- | Dummy value for generated data, where a true source position does
--   not exist.
noPos :: Posn
noPos = Pn "no recorded position" 0 0 Nothing

-- | @posInNewCxt name pos@ creates a new source position from an old one.
--   It is used when opening a new file (e.g. a DTD inclusion), to denote
--   the start of the file @name@, but retain the stacked information that
--   it was included from the old @pos@.
posInNewCxt :: String -> Maybe Posn -> Posn
posInNewCxt name pos = Pn name 1 1 pos

instance Show Posn where
      showsPrec _ (Pn f l c i) = showString "file " .
                                 showString f .
                                 showString "  at line " . shows l .
                                 showString " col " . shows c .
                                 ( case i of
                                    Nothing -> id
                                    Just p  -> showString "\n    used by  " .
                                               shows p )

-- | Just used to strictify the internal values of a position, to avoid
--   space leaks.
forcep :: Posn -> Int
forcep (Pn _ n m _) = m `seq` n

-- | Add n character positions to the given position.
addcol :: Int -> Posn -> Posn
addcol n (Pn f r c i) = Pn f r (c+n) i

-- | Add a newline or tab to the given position.
newline, tab :: Posn -> Posn
newline (Pn f r _ i) = Pn f (r+1) 1 i
tab     (Pn f r c i) = Pn f r (((c`div`8)+1)*8) i

-- | Add the given whitespace char to the given position.
--   Precondition: @white c | isSpace c = True@
white :: Char -> Posn -> Posn
white ' '    = addcol 1
white '\n'   = newline
white '\r'   = id
white '\t'   = tab
white '\xa0' = addcol 1
white x | isSpace x = addcol 1 -- other Unicode whitespace
white _      = error "precondition not satisfied: Posn.white c | isSpace c"
