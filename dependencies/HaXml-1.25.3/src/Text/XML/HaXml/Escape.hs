{- This module contains code for escaping/unescaping text in attributes
   and elements in the HaXml Element type, replacing characters by character
   references or vice-versa.  Two uses are envisaged for this:

   (1) stopping HaXml generating incorrect XML when a character is included
       which is also the appropriate XML terminating character, for example
       when an attribute includes a double quote.
   (2) representing XML which contains non-ASCII characters as ASCII.
   -}
module Text.XML.HaXml.Escape(
   xmlEscape,
      -- :: XmlEscaper -> Element i -> Element i
   xmlUnEscape,
      -- :: XmlEscaper -> Element i -> Element i
   xmlEscapeContent,
      -- :: XmlEscaper -> [Content i] -> [Content i]
   xmlUnEscapeContent,
      -- :: XmlEscaper -> [Content i] -> [Content i]

   XmlEscaper,
      -- Something describing a particular set of escapes.

   stdXmlEscaper,
      -- Standard boilerplate escaper, escaping everything that is
      -- nonprintable, non-ASCII, or might conceivably cause problems by
      -- parsing XML, for example quotes, < signs, and ampersands.

   mkXmlEscaper,
      -- :: [(Char,String)] -> (Char -> Bool) -> XmlEscaper
      -- The first argument contains a list of characters, with their
      --    corresponding character reference names.
      --    For example [('\60',"lt"),('\62',"gt"),('\38',"amp"),
      --       ('\39',"apos"),('\34',"quot")] will give you the "standard"
      --       XML escapes listed in section 4.6 of the XML standard, so that
      --       "&quot;" will automatically get translated into a double
      --       quotation mark.
      --
      --       It's the caller's responsibility to see that the reference
      --       names ("lt","gt","amp","apos" and "quot" in the above example)
      --       are valid XML reference names.  A sequence of letters, digits,
      --       "." or ":" characters should be fine so long as the first one
      --       isn't a digit.
      --
      -- The second argument is a function applied to each text character.
      --    If it returns True, that means we should escape this character.

      -- Policy: on escaping, we expand all characters for which the
      -- (Char -> Bool) function returns True, either giving the corresponding
      -- character reference name if one was supplied, or else using a
      -- hexadecimal CharRef.
      --
      -- on unescaping, we translate all the references we understand
      --   (hexadecimal,decimal, and the ones in the [(Char,String)] list,
      --   and leave the others alone.

   ) where

import Data.Char
-- import Numeric
import Text.XML.HaXml.Types

#if __GLASGOW_HASKELL__ >= 604 || __NHC__ >= 118 || defined(__HUGS__)
-- emulate older finite map interface using Data.Map, if it is available
import qualified Data.Map as Map
type FiniteMap a b = Map.Map a b
listToFM :: Ord a => [(a,b)] -> FiniteMap a b
listToFM = Map.fromList
lookupFM :: Ord a => FiniteMap a b -> a -> Maybe b
lookupFM = flip Map.lookup
#elif __GLASGOW_HASKELL__ >= 504 || __NHC__ > 114
-- real finite map, if it is available
import Data.FiniteMap
#else
-- otherwise, a very simple and inefficient implementation of a finite map
type FiniteMap a b = [(a,b)]
listToFM :: Eq a => [(a,b)] -> FiniteMap a b
listToFM = id
lookupFM :: Eq a => FiniteMap a b -> a -> Maybe b
lookupFM fm k = lookup k fm
#endif


-- ------------------------------------------------------------------------
-- Data types
-- ------------------------------------------------------------------------

data XmlEscaper = XmlEscaper {
   toEscape :: FiniteMap Char String,
   fromEscape :: FiniteMap String Char,
   isEscape :: Char -> Bool
   }


-- ------------------------------------------------------------------------
-- Escaping
-- ------------------------------------------------------------------------



xmlEscape :: XmlEscaper -> Element i -> Element i
xmlEscape xmlEscaper element =
   compressElement (escapeElement xmlEscaper element)

xmlEscapeContent :: XmlEscaper -> [Content i] -> [Content i]
xmlEscapeContent xmlEscaper cs =
   compressContent (escapeContent xmlEscaper cs)

escapeElement :: XmlEscaper -> Element i -> Element i
escapeElement xmlEscaper (Elem name attributes content) =
   Elem name (escapeAttributes xmlEscaper attributes)
      (escapeContent xmlEscaper content)

escapeAttributes :: XmlEscaper -> [Attribute] -> [Attribute]
escapeAttributes xmlEscaper atts =
   map
      (\ (name,av) -> (name,escapeAttValue xmlEscaper av))
      atts

escapeAttValue :: XmlEscaper -> AttValue -> AttValue
escapeAttValue xmlEscaper (AttValue attValList) =
   AttValue (
      concat (
         map
            (\ av -> case av of
               Right _ -> [av]
               Left s ->
                  map
                     (\ c -> if isEscape xmlEscaper c
                        then
                           Right (mkEscape xmlEscaper c)
                        else
                           Left [c]
                        )
                     s
               )
            attValList
         )
      )

escapeContent :: XmlEscaper -> [Content i] -> [Content i]
escapeContent xmlEscaper contents =
   concat
      (map
          (\ content -> case content of
             (CString b str i) ->
                map
                   (\ c -> if isEscape xmlEscaper c
                      then
                         CRef (mkEscape xmlEscaper c) i
                      else
                         CString b [c] i
                      )
                   str
             (CElem element i) -> [CElem (escapeElement xmlEscaper element) i]
             _ -> [content]
             )
          contents
          )

mkEscape :: XmlEscaper -> Char -> Reference
mkEscape (XmlEscaper {toEscape = toescape}) ch =
   case lookupFM toescape ch of
      Nothing  -> RefChar (ord ch)
      Just str -> RefEntity str
--    where
--       _ = showIntAtBase 16 intToDigit
--       -- It should be, but in GHC it isn't.

-- ------------------------------------------------------------------------
-- Unescaping
-- ------------------------------------------------------------------------

xmlUnEscape :: XmlEscaper -> Element i -> Element i
xmlUnEscape xmlEscaper element =
   compressElement (unEscapeElement xmlEscaper element)

xmlUnEscapeContent :: XmlEscaper -> [Content i] -> [Content i]
xmlUnEscapeContent xmlEscaper cs =
   compressContent (unEscapeContent xmlEscaper cs)

unEscapeElement :: XmlEscaper -> Element i -> Element i
unEscapeElement xmlEscaper (Elem name attributes content) =
   Elem name (unEscapeAttributes xmlEscaper attributes)
      (unEscapeContent xmlEscaper content)

unEscapeAttributes :: XmlEscaper -> [Attribute] -> [Attribute]
unEscapeAttributes xmlEscaper atts =
   map
      (\ (name,av) -> (name,unEscapeAttValue xmlEscaper av))
      atts

unEscapeAttValue :: XmlEscaper -> AttValue -> AttValue
unEscapeAttValue xmlEscaper (AttValue attValList) =
   AttValue (
      map
         (\ av -> case av of
            Left _ -> av
            Right ref -> case unEscapeChar xmlEscaper ref of
               Just c -> Left [c]
               Nothing -> av
            )
         attValList
      )

unEscapeContent :: XmlEscaper -> [Content i] -> [Content i]
unEscapeContent xmlEscaper content =
   map
      (\ cntnt -> case cntnt of
         CRef ref i -> case unEscapeChar xmlEscaper ref of
            Just c -> CString False [c] i
            Nothing -> cntnt
         CElem element i -> CElem (unEscapeElement xmlEscaper element) i
         _ -> cntnt
         )
      content

unEscapeChar :: XmlEscaper -> Reference -> Maybe Char
unEscapeChar xmlEscaper ref =
   case ref of
      RefChar i      -> Just (chr i)
      RefEntity name -> lookupFM (fromEscape xmlEscaper) name

-- ------------------------------------------------------------------------
-- After escaping and unescaping we rebuild the lists, compressing
-- adjacent identical character data.
-- ------------------------------------------------------------------------

compressElement :: Element i -> Element i
compressElement (Elem name attributes content) =
   Elem name (compressAttributes attributes) (compressContent content)

compressAttributes :: [(QName,AttValue)] -> [(QName,AttValue)]
compressAttributes atts =
   map
      (\ (name,av) -> (name,compressAttValue av))
      atts

compressAttValue :: AttValue -> AttValue
compressAttValue (AttValue l) = AttValue (compress l)
   where
      compress :: [Either String Reference] -> [Either String Reference]
      compress [] = []
      compress (Right ref : es) = Right ref : (compress es)
      compress ( (ls @ (Left s1)) : es) =
         case compress es of
            (Left s2 : es2) -> Left (s1 ++ s2) : es2
            es2 -> ls : es2

compressContent :: [Content i] -> [Content i]
compressContent [] = []
compressContent ((csb @ (CString b1 s1 i1)) : cs) =
   case compressContent cs of
      (CString b2 s2 _) : cs2
          | b1 == b2
          -> CString b1 (s1 ++ s2) i1: cs2
      cs2 -> csb : cs2
compressContent (CElem element i : cs) =
   CElem (compressElement element) i : compressContent cs
compressContent (c : cs) = c : compressContent cs


-- ------------------------------------------------------------------------
-- Making XmlEscaper values.
-- ------------------------------------------------------------------------

stdXmlEscaper :: XmlEscaper
stdXmlEscaper = mkXmlEscaper
   [('\60',"lt"),('\62',"gt"),('\38',"amp"),('\39',"apos"),('\34',"quot")]
   (\ ch ->
      let
         i = ord ch
      in
         i < 10 || (10<i && i<32) || i >= 127 ||
            case ch of
               '\'' -> True
               '\"' -> True
               '&' -> True
               '<' -> True
               '>' -> True
               _ -> False
      )


mkXmlEscaper :: [(Char,String)] -> (Char -> Bool) -> XmlEscaper
mkXmlEscaper escapes isescape =
   XmlEscaper {
      toEscape = listToFM escapes,
      fromEscape = listToFM (map (\ (c,str) -> (str,c)) escapes),
      isEscape = isescape
      }

