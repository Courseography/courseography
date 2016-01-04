-- | The class 'XmlContent' is a kind of replacement for Read and Show:
--   it provides conversions between a generic XML tree representation
--   and your own more specialised typeful Haskell data trees.
--
--   If you are starting with a set of Haskell datatypes, use DrIFT to
--   derive instances of this class for you:
--       http:\/\/repetae.net\/john\/computer\/haskell\/DrIFT
--   and use the current module for instances of the standard Haskell
--   datatypes list, Maybe, and so on.
--
--   If you are starting with an XML DTD, use HaXml's tool DtdToHaskell
--   to generate both the Haskell types and the corresponding instances,
--   but _do_not_ use the current module for instances: use
--   Text.XML.HaXml.XmlContent instead.

module Text.XML.HaXml.XmlContent.Haskell
  (
  -- * Re-export everything from Text.XML.HaXml.XmlContent.Parser.
    module Text.XML.HaXml.XmlContent.Parser
  -- * Instances (only) for the XmlContent class, for datatypes that
  --   originated in Haskell, rather than from a DTD definition.
--  , module Text.XML.HaXml.XmlContent.Haskell

  -- * Whole-document conversion functions
  , toXml, fromXml
  , readXml, showXml, fpsShowXml
  , fReadXml, fWriteXml, fpsWriteXml
  , hGetXml,  hPutXml, fpsHPutXml

  ) where

import System.IO
import Data.List (isPrefixOf, isSuffixOf)
import qualified Text.XML.HaXml.ByteStringPP as FPS (document)
import qualified Data.ByteString.Lazy.Char8 as FPS

import Text.PrettyPrint.HughesPJ (render)
import Text.ParserCombinators.Poly

import Text.XML.HaXml.Types
import Text.XML.HaXml.Namespaces
import Text.XML.HaXml.TypeMapping
import Text.XML.HaXml.Posn     (Posn, posInNewCxt)
import Text.XML.HaXml.Pretty   (document)
import Text.XML.HaXml.Parse    (xmlParse)
import Text.XML.HaXml.Verbatim (Verbatim(verbatim))
import Text.XML.HaXml.XmlContent.Parser


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
             ( case (ht, toContents value) of
                 (Tuple _, cs)       -> Elem (N $ showHType ht "") [] cs
                 (Defined _ _ _, cs) -> Elem (N $ showHType ht "-XML") [] cs
                 (_, [CElem e ()])   -> e )
             []

-- | Read a Haskell value from an XML document, ignoring the DTD and
--   using the Haskell result type to determine how to parse it.
fromXml :: XmlContent a => Document Posn -> Either String a
fromXml (Document _ _ e@(Elem n _ cs) _)
  | "tuple" `isPrefixOf` localName n = fst (runParser parseContents cs)
  | "-XML"  `isSuffixOf` localName n = fst (runParser parseContents cs)
  | otherwise = fst (runParser parseContents
                               [CElem e (posInNewCxt "document" Nothing)])


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
-- These are for Haskell datatypes being derived to go to XML.
-- DtdToHaskell does not use these instances.
------------------------------------------------------------------------

instance XmlContent Bool where
    toContents b   = [CElem (Elem (N "bool") [mkAttr "value" (show b)] []) ()]
    parseContents = do { e <- element ["bool"] ; return (attval e) }

instance XmlContent Int where
    toContents i   = [CElem (Elem (N "int") [mkAttr "value" (show i)] []) ()]
    parseContents = do { e <- element ["int"] ; return (attval e) }

instance XmlContent Integer where
    toContents i   = [CElem (Elem (N "integer") [mkAttr "value" (show i)] []) ()]
    parseContents = do { e <- element ["integer"] ; return (attval e) }

instance XmlContent Float where
    toContents i   = [CElem (Elem (N "float") [mkAttr "value" (show i)] []) ()]
    parseContents = do { e <- element ["float"] ; return (attval e) }

instance XmlContent Double where
    toContents i   = [CElem (Elem (N "double") [mkAttr "value" (show i)] []) ()]
    parseContents = do { e <- element ["double"] ; return (attval e) }

instance XmlContent Char where
    -- NOT in a string
    toContents c   = [CElem (Elem (N "char") [mkAttr "value" [c]] []) ()]
    parseContents = do { (Elem _ [(N "value",(AttValue [Left [c]]))] [])
                             <- element ["char"]
                       ; return c
                       }
    -- Only defined for Char and no other types:
    xToChar   = id
    xFromChar = id

instance XmlContent a => XmlContent [a] where
    toContents xs  = case toHType x of
                       (Prim "Char" _) ->
                            [mkElem "string" [CString True (map xToChar xs) ()]]
                       _ -> [mkElem xs (concatMap toContents xs)]
                   where   (x:_) = xs
    parseContents = P (\x ->
        case x of
            (CString _ s _:cs)
                   -> Success cs (map xFromChar s)
            (CElem (Elem (N "string") [] [CString _ s _]) _:cs)
                   -> Success cs (map xFromChar s)
            (CElem (Elem (N "string") [] []) _:cs)
                   -> Success cs []
            (CElem (Elem (N e) [] xs) _:cs) | "list" `isPrefixOf` e
                   -> scanElements xs
                   where
                  -- scanElements :: [Content] -> (Either String [a],[Content])
                     scanElements [] = Success cs []
                     scanElements es =
                        case runParser parseContents es of
                            (Left msg, es') -> Failure es' msg
                            (Right y, es') ->
                                case scanElements es' of
                                    Failure ds msg -> Failure ds msg
                                    Success ds ys  -> Success ds (y:ys)
            (CElem (Elem e _ _) pos: cs)
                   -> Failure cs ("Expected a <list-...>, but found a <"
                                  ++printableName e
                                  ++"> at\n"++show pos)
            (CRef r pos: cs)
                   -> Failure cs ("Expected a <list-...>, but found a ref "
                                  ++verbatim r++" at\n"++ show pos)
            (_:cs) -> ((\ (P p)-> p) parseContents) cs  -- skip comments etc.
            []     -> Failure [] "Ran out of input XML whilst secondary parsing"
        )

instance XmlContent () where
    toContents ()  = [CElem (Elem (N "unit") [] []) ()]
    parseContents = do { element ["unit"]; return () }


instance (XmlContent a) => XmlContent (Maybe a) where
    toContents m   = [mkElem m (maybe [] toContents m)]
    parseContents = do
        { e <- elementWith (flip isPrefixOf) ["maybe"]
        ; case e of (Elem _ [] []) -> return Nothing
                    (Elem _ [] _)  -> fmap Just (interior e parseContents)
        }

instance (XmlContent a, XmlContent b) => XmlContent (Either a b) where
    toContents v@(Left aa) =
        [mkElemC (showConstr 0 (toHType v)) (toContents aa)]
    toContents v@(Right ab) =
        [mkElemC (showConstr 1 (toHType v)) (toContents ab)]
    parseContents =
        (inElementWith (flip isPrefixOf) "Left"  $ fmap Left  parseContents)
          `onFail`
        (inElementWith (flip isPrefixOf) "Right" $ fmap Right parseContents)

--    do{ e@(Elem t [] _) <- element ["Left","Right"]
--      ; case t of
--          _ | "Left"  `isPrefixOf` t -> fmap Left  (interior e parseContents)
--            | "Right" `isPrefixOf` t -> fmap Right (interior e parseContents)
--      }

------------------------------------------------------------------------
