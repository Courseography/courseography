-- | The class 'XmlContent' is a kind of replacement for Read and Show:
--   it provides conversions between a generic XML tree representation
--   and your own more specialised typeful Haskell data trees.
--
--   If you are starting with an XML DTD, use HaXml's tool DtdToHaskell
--   to generate both the Haskell types and the corresponding instances.
--   
--   If you are starting with a set of Haskell datatypes, use DrIFT to
--   derive instances of this class for you:
--       http:\/\/repetae.net\/john\/computer\/haskell\/DrIFT
--   and _do_not_ use the current module, but rather
--   Text.XML.HaXml.XmlContent.Haskell, for the correct matching
--   instances for standard Haskell datatypes.

module Text.XML.HaXml.XmlContent
  (
  -- * Re-export everything from Text.XML.HaXml.XmlContent.Parser.
    module Text.XML.HaXml.XmlContent.Parser
  , module Text.XML.HaXml.TypeMapping
  -- * Contains instances of the XmlContent classes,
  --   for the basic Haskell datatypes list and Maybe,
  --   intended for use with DtdToHaskell-generated datatypes.
  --   See the alternative instances in Text.XML.HaXml.XmlContent.Haskell
  --   if your datatypes originate in Haskell instead.
--  , module Text.XML.HaXml.XmlContent

  -- * Whole-document conversion functions
  , toXml, fromXml
  , readXml, showXml, fpsShowXml
  , fReadXml, fWriteXml, fpsWriteXml
  , hGetXml,  hPutXml, fpsHPutXml
  ) where

import System.IO
import qualified Text.XML.HaXml.ByteStringPP as FPS (document)
import qualified Data.ByteString.Lazy.Char8 as FPS

import Text.PrettyPrint.HughesPJ (render)
--import Text.ParserCombinators.Poly

import Text.XML.HaXml.Types
import Text.XML.HaXml.TypeMapping
import Text.XML.HaXml.Posn     (Posn, posInNewCxt)
import Text.XML.HaXml.Pretty   (document)
import Text.XML.HaXml.Parse    (xmlParse)
import Text.XML.HaXml.XmlContent.Parser


------------------------------------------------------------------------

        -- probably want to write DTD separately from value, and have
        -- easy ways to combine DTD + value into a document, or write
        -- them to separate files.

-- | Read an XML document from a file and convert it to a fully-typed
--   Haskell value.
fReadXml  :: XmlContent a => FilePath -> IO a
fReadXml fp = do
    f <- ( if fp=="-" then return stdin
           else openFile fp ReadMode )
    x <- hGetContents f
    let (Document _ _ y _) = xmlParse fp x
        y' = CElem y (posInNewCxt fp Nothing)
    either fail return (fst (runParser parseContents [y']))

-- | Write a fully-typed Haskell value to the given file as an XML
--   document.
fWriteXml :: XmlContent a => FilePath -> a -> IO ()
fWriteXml fp x = do
    f <- ( if fp=="-" then return stdout
           else openFile fp WriteMode )
    hPutXml f False x
    hClose f

-- | Write any Haskell value to the given file as an XML document,
--   using the FastPackedString interface (output will not be prettified).
fpsWriteXml :: XmlContent a => FilePath -> a -> IO ()
fpsWriteXml fp x = do
    f <- ( if fp=="-" then return stdout
           else openFile fp WriteMode )
    fpsHPutXml f False x
    hClose f

-- | Read a fully-typed XML document from a string.
readXml :: XmlContent a => String -> Either String a
readXml s =
    let (Document _ _ y _) = xmlParse "string input" s in
    fst (runParser parseContents
                   [CElem y (posInNewCxt "string input" Nothing)])

-- | Convert a fully-typed XML document to a string (without DTD).
showXml :: XmlContent a => Bool -> a -> String
showXml dtd x =
    case toContents x of
      [CElem _ _] -> (render . document . toXml dtd) x
      _ -> ""

-- | Convert a fully-typed XML document to a ByteString (without DTD).
fpsShowXml :: XmlContent a => Bool -> a -> FPS.ByteString
fpsShowXml dtd x =
    case toContents x of
      [CElem _ _] -> (FPS.document . toXml dtd) x
      _ -> FPS.empty


-- | Convert a fully-typed XML document to a string (with or without DTD).
toXml :: XmlContent a => Bool -> a -> Document ()
toXml dtd value =
    let ht = toHType value in
    Document (Prolog (Just (XMLDecl "1.0" Nothing Nothing))
                     [] (if dtd then Just (toDTD ht) else Nothing) [])
             emptyST
             ( case toContents value of
                 []             -> Elem (N "empty") [] []
                 [CElem e ()]   -> e
                 (CElem _ ():_) -> error "too many XML elements in document" )
             []

-- | Read a Haskell value from an XML document, ignoring the DTD and
--   using the Haskell result type to determine how to parse it.
fromXml :: XmlContent a => Document Posn -> Either String a
fromXml (Document _ _ e@(Elem _ _ _) _) =
  fst (runParser parseContents [CElem e (posInNewCxt "document" Nothing)])


-- | Read a fully-typed XML document from a file handle.
hGetXml :: XmlContent a => Handle -> IO a
hGetXml h = do
    x <- hGetContents h
    let (Document _ _ y _) = xmlParse "file handle" x
    either fail return
           (fst (runParser parseContents
                           [CElem y (posInNewCxt "file handle" Nothing)]))

-- | Write a fully-typed XML document to a file handle.
hPutXml :: XmlContent a => Handle -> Bool -> a -> IO ()
hPutXml h dtd x = do
    (hPutStrLn h . render . document . toXml dtd) x

-- | Write a fully-typed XML document to a file handle, using the
--   FastPackedString interface (output will not be prettified).
fpsHPutXml :: XmlContent a => Handle -> Bool -> a -> IO ()
fpsHPutXml h dtd x = do
    (FPS.hPut h . FPS.document . toXml dtd) x


------------------------------------------------------------------------
-- Instances for all the standard basic datatypes.
-- DtdToHaskell uses only a small number of standard datatypes.
------------------------------------------------------------------------

instance XmlContent Char where
    -- NOT in a string
    toContents _  = error $ "Text.XML.HaXml.XmlContent.toContents "++
                            " used on a Haskell Char"
    parseContents = fail  $ "Text.XML.HaXml.XmlContent.parseContents "++
                            " used on a Haskell Char "
    -- Only defined for Char and no other types:
    xToChar   = id
    xFromChar = id

instance XmlContent a => XmlContent [a] where
    toContents xs  = case toHType x of
                       (Prim "Char" _) ->
                            [CString True (map xToChar xs) ()]
                       _ -> concatMap toContents xs
                   where   (x:_) = xs
    parseContents = let result = runParser p [] -- for type of result only
                        p = case (toHType . head . (\ (Right x)->x) . fst)
                                 result of
                              (Prim "Char" _) -> fmap (map xFromChar) $ text
                              _ -> many parseContents
                    in p
        -- comments, PIs, etc, are skipped in the individual element parser.

instance (XmlContent a) => XmlContent (Maybe a) where
    toContents m  = maybe [] toContents m
    parseContents = optional parseContents

------------------------------------------------------------------------
