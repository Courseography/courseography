{- |
 - Only a small module containing some helper functions to extract xml content
 - I would have added this to Types but I've put it into an additional module
 - to avoid circular references (Verbatim <-> Types)
-}

module Text.XML.HaXml.Util
  (
  -- ** utility functions to access XML content
    docContent
  , contentElem
  , attrs
  , tagTextContent
  ) where

--import Text.XML.HaXml.Posn
import Text.XML.HaXml.Types
import Text.XML.HaXml.Verbatim

-- | Get the main element of the document so that you can apply
--   CFilters directly.  'i' is typically (posInNewCxt "filename" Nothing) 
docContent :: i -> Document i -> Content i
docContent i (Document _ _ e _) = CElem e i

-- | If you know that your CFilter returns a tag, you can use this
--   function to get the tagged Element.
contentElem ::  Content i -> Element i
contentElem (CElem e _) = e
contentElem _ = error "content is not a CElem"

attrs :: Element i -> [Attribute]
attrs ( Elem _ attrs _ ) = attrs

tagTextContent :: Content i -> [Char]
tagTextContent ((CElem (Elem _ _ cs) _)) = concatMap verbatim cs 

{-
  now you can extract an attribute quite easily:
    let doc = "<xml><a><b a=\"x\">content</b></a></xml>"
    let b = head $ xtract id "a/b" $ docContent (posInNewCxt filename Nothing) $ xmlParse filename doc
    putStrLn $ "attr a of tag b" ++ (show $ lookup "a" $  attrs $ contentElem b)
    putStrLn $ "text content of b :" ++ tagTextContent b
  still (too) much code IMHO
-}

