-- | You don't normally need to use this Lex module directly - it is
--   called automatically by the parser.  (This interface is only exposed
--   for debugging purposes.)
--
-- This is a hand-written lexer for tokenising the text of an XML
-- document so that it is ready for parsing.  It attaches position
-- information in (line,column) format to every token.  The main
-- entry point is 'xmlLex'.  A secondary entry point, 'xmlReLex', is
-- provided for when the parser needs to stuff a string back onto
-- the front of the text and re-tokenise it (typically when expanding
-- macros).
--
-- As one would expect, the lexer is essentially a small finite
-- state machine.

module Text.XML.HaXml.Lex
  (
  -- * Entry points to the lexer
    xmlLex         -- :: String -> String -> [Token]
  , xmlReLex       -- :: Posn   -> String -> [Token]
  , reLexEntityValue -- :: (String->Maybe String) -> Posn -> String -> [Token]
  -- * Token types
  , Token
  , TokenT(..)
  , Special(..)
  , Section(..)
  ) where

import Data.Char
import Text.XML.HaXml.Posn

data Where = InTag String | NotInTag
    deriving (Eq)

-- | All tokens are paired up with a source position.
--   Lexical errors are passed back as a special @TokenT@ value.
type Token = (Posn, TokenT)

-- | The basic token type.
data TokenT =
      TokCommentOpen		-- ^   \<!--
    | TokCommentClose		-- ^   -->
    | TokPIOpen			-- ^   \<?
    | TokPIClose		-- ^   ?>
    | TokSectionOpen		-- ^   \<![
    | TokSectionClose		-- ^   ]]>
    | TokSection Section	-- ^   CDATA INCLUDE IGNORE etc
    | TokSpecialOpen		-- ^   \<!
    | TokSpecial Special	-- ^   DOCTYPE ELEMENT ATTLIST etc
    | TokEndOpen		-- ^   \<\/
    | TokEndClose		-- ^   \/>
    | TokAnyOpen		-- ^   \<
    | TokAnyClose		-- ^   >
    | TokSqOpen			-- ^   \[
    | TokSqClose		-- ^   \]
    | TokEqual			-- ^   =
    | TokQuery			-- ^   ?
    | TokStar			-- ^   \*
    | TokPlus			-- ^   +
    | TokAmp			-- ^   &
    | TokSemi			-- ^   ;
    | TokHash			-- ^   #
    | TokBraOpen		-- ^   (
    | TokBraClose		-- ^   )
    | TokPipe			-- ^   |
    | TokPercent		-- ^   %
    | TokComma			-- ^   ,
    | TokQuote			-- ^   \'\' or \"\"
    | TokName      String	-- ^   begins with letter, no spaces
    | TokFreeText  String	-- ^   any character data
    | TokNull			-- ^   fake token
    | TokError     String	-- ^   lexical error
    deriving (Eq)

data Special =
      DOCTYPEx
    | ELEMENTx
    | ATTLISTx
    | ENTITYx
    | NOTATIONx
    deriving (Eq,Show)
data Section =
      CDATAx
    | INCLUDEx
    | IGNOREx
    deriving (Eq,Show)

instance Show TokenT where
  showsPrec _p TokCommentOpen		= showString     "<!--"
  showsPrec _p TokCommentClose		= showString     "-->"
  showsPrec _p TokPIOpen		= showString     "<?"
  showsPrec _p TokPIClose		= showString     "?>"
  showsPrec _p TokSectionOpen		= showString     "<!["
  showsPrec _p TokSectionClose		= showString     "]]>"
  showsPrec  p (TokSection s)		= showsPrec p s
  showsPrec _p TokSpecialOpen		= showString     "<!"
  showsPrec  p (TokSpecial s)		= showsPrec p s
  showsPrec _p TokEndOpen		= showString     "</"
  showsPrec _p TokEndClose		= showString     "/>"
  showsPrec _p TokAnyOpen		= showString     "<"
  showsPrec _p TokAnyClose		= showString     ">"
  showsPrec _p TokSqOpen		= showString     "["
  showsPrec _p TokSqClose		= showString     "]"
  showsPrec _p TokEqual			= showString     "="
  showsPrec _p TokQuery			= showString     "?"
  showsPrec _p TokStar			= showString     "*"
  showsPrec _p TokPlus			= showString     "+"
  showsPrec _p TokAmp			= showString     "&"
  showsPrec _p TokSemi			= showString     ";"
  showsPrec _p TokHash			= showString     "#"
  showsPrec _p TokBraOpen		= showString     "("
  showsPrec _p TokBraClose		= showString     ")"
  showsPrec _p TokPipe			= showString     "|"
  showsPrec _p TokPercent		= showString     "%"
  showsPrec _p TokComma			= showString     ","
  showsPrec _p TokQuote			= showString     "' or \""
  showsPrec _p (TokName      s)		= showString     s
  showsPrec _p (TokFreeText  s)		= showString     s
  showsPrec _p TokNull			= showString     "(null)"
  showsPrec _p (TokError     s)		= showString     s

--trim, revtrim :: String -> String
--trim    = f . f         where f = reverse . dropWhile isSpace
--revtrim = f.reverse.f   where f = dropWhile isSpace
--revtrim = reverse . dropWhile (=='\n')  -- most recently used defn.

emit :: TokenT -> Posn -> Token
emit tok p = forcep p `seq` (p,tok)

lexerror :: String -> Posn -> [Token]
lexerror s p = [(p, TokError ("Lexical error:\n  "++s))]

skip :: Int -> Posn -> String -> (Posn->String->[Token]) -> [Token]
skip n p s k = k (addcol n p) (drop n s)

blank :: ([Where]->Posn->String->[Token]) -> [Where]-> Posn-> String-> [Token]
blank _  (InTag t:_) p [] = lexerror ("unexpected EOF within "++t) p
blank _          _   _ [] = []
blank k      w p (' ': s) = blank k w (addcol 1 p) s
blank k      w p ('\t':s) = blank k w (tab p) s
blank k      w p ('\n':s) = blank k w (newline p) s
blank k      w p ('\r':s) = blank k w  p s
blank k   w p ('\xa0': s) = blank k w (addcol 1 p) s
blank k      w p    s     = k w p s

prefixes :: String -> String -> Bool
[]     `prefixes`   _    = True
(x:xs) `prefixes` (y:ys) = x==y && xs `prefixes` ys
(_:_)  `prefixes`   []   = False --error "unexpected EOF in prefix"

textUntil, textOrRefUntil
    :: [Char] -> TokenT -> [Char] -> Posn -> Posn -> [Char]
       -> (Posn->String->[Token]) -> [Token]

textUntil close _tok _acc pos p [] _k =
    lexerror ("unexpected EOF while looking for closing token "++close
              ++"\n  to match the opening token in "++show pos) p
textUntil close  tok  acc pos p (s:ss) k
    | close `prefixes` (s:ss)  = emit (TokFreeText (reverse acc)) pos:
                                 emit tok p:
                                 skip (length close-1) (addcol 1 p) ss k
    | tok==TokSemi && length acc >= 8 -- special case for repairing broken &
                               = emit (TokFreeText "amp") pos:
                                 emit tok pos:
                                 k (addcol 1 pos) (reverse acc++s:ss)
    | isSpace s  = textUntil close tok (s:acc) pos (white s p) ss k
    | otherwise  = textUntil close tok (s:acc) pos (addcol 1 p) ss k

textOrRefUntil close _tok _acc pos p [] _k =
    lexerror ("unexpected EOF while looking for closing token "++close
              ++"\n  to match the opening token in "++show pos) p
textOrRefUntil close  tok  acc pos p (s:ss) k
    | close `prefixes` (s:ss)  = emit (TokFreeText (reverse acc)) pos:
                                 emit tok p:
                                 skip (length close-1) (addcol 1 p) ss k
    | s=='&'     = (if not (null acc)
                       then (emit (TokFreeText (reverse acc)) pos:)
                       else id)
                   (emit TokAmp p:
                    textUntil ";" TokSemi "" p (addcol 1 p) ss
                        (\p' i-> textOrRefUntil close tok "" p p' i k))
    | isSpace s  = textOrRefUntil close tok (s:acc) pos (white s p) ss k
    | otherwise  = textOrRefUntil close tok (s:acc) pos (addcol 1 p) ss k

----

-- | The first argument to 'xmlLex' is the filename (used for source positions,
--   especially in error messages), and the second is the string content of
--   the XML file.
xmlLex :: String -> String -> [Token]
xmlLex filename = xmlAny [] (posInNewCxt filename Nothing)

-- | 'xmlReLex' is used when the parser expands a macro (PE reference).
--    The expansion of the macro must be re-lexed as if for the first time.
xmlReLex :: Posn -> String -> [Token]
xmlReLex p s
      | "INCLUDE"  `prefixes` s  = emit (TokSection INCLUDEx) p: k 7
      | "IGNORE"   `prefixes` s  = emit (TokSection IGNOREx) p:  k 6
      | otherwise = blank xmlAny [] p s
  where
    k n = skip n p s (blank xmlAny [])

-- | 'reLexEntityValue' is used solely within parsing an entityvalue.
--   Normally, a PERef is logically separated from its surroundings by
--   whitespace.  But in an entityvalue, a PERef can be juxtaposed to
--   an identifier, so the expansion forms a new identifier.
--   Thus the need to rescan the whole text for possible PERefs.
reLexEntityValue :: (String->Maybe String) -> Posn -> String -> [Token]
reLexEntityValue lookup p s =
    textOrRefUntil "%" TokNull [] p p (expand s++"%") (xmlAny [])
  where
    expand []       = []
    expand ('%':xs) = let (sym,rest) = break (==';') xs in
                      case lookup sym of
                        Just val -> expand val ++ expand (tail rest)
                        Nothing  -> "%"++sym++";"++ expand (tail rest) -- hmmm
    expand (x:xs)   = x: expand xs

--xmltop :: Posn -> String -> [Token]
--xmltop p [] = []
--xmltop p s
--    | "<?"   `prefixes` s = emit TokPIOpen p:      next 2 (xmlPI [InTag "<?...?>"])
--    | "<!--" `prefixes` s = emit TokCommentOpen p: next 4 (xmlComment [])
--    | "<!"   `prefixes` s = emit TokSpecialOpen p: next 2 (xmlSpecial [InTag "<!...>"])
--    | otherwise           = lexerror "expected <?xml?> or <!DOCTYPE>" p
--  where next n k = skip n p s k

xmlPI, xmlPIEnd, xmlComment, xmlAny, xmlTag, xmlSection, xmlSpecial
    :: [Where] -> Posn -> String -> [Token]

xmlPI      w p s = xmlName p s "name of processor in <? ?>" (blank xmlPIEnd w)
xmlPIEnd   w p s = textUntil "?>"  TokPIClose "" p p s (blank xmlAny (tail w))
xmlComment w p s = textUntil "-->" TokCommentClose "" p p s (blank xmlAny w)

-- Note: the order of the clauses in xmlAny is very important.
-- Some matches must precede the NotInTag test, the rest must follow it.
xmlAny  (InTag t:_)  p [] = lexerror ("unexpected EOF within "++t) p
xmlAny          _    _ [] = []
xmlAny w p s@('<':ss)
    | "?"   `prefixes` ss = emit TokPIOpen p:
                                         skip 2 p s (xmlPI (InTag "<?...?>":w))
    | "!--" `prefixes` ss = emit TokCommentOpen p: skip 4 p s (xmlComment w)
    | "!["  `prefixes` ss = emit TokSectionOpen p: skip 3 p s (xmlSection w)
    | "!"   `prefixes` ss = emit TokSpecialOpen p:
                                     skip 2 p s (xmlSpecial (InTag "<!...>":w))
    | "/"   `prefixes` ss = emit TokEndOpen p: 
                                    skip 2 p s (xmlTag (InTag "</...>":tail w))
    | otherwise           = emit TokAnyOpen p:
                                 skip 1 p s (xmlTag (InTag "<...>":NotInTag:w))
xmlAny (_:_:w) p s@('/':ss)
    | ">"   `prefixes` ss = emit TokEndClose p: skip 2 p s (xmlAny w)
xmlAny w p ('&':ss) = emit TokAmp p:      textUntil ";" TokSemi "" p
                                                     (addcol 1 p) ss (xmlAny w)
xmlAny w@(NotInTag:_) p s = xmlContent "" w p p s
-- everything below here is implicitly InTag.
xmlAny w p ('>':ss) = emit TokAnyClose p: xmlAny (tail w) (addcol 1 p) ss
xmlAny w p ('[':ss) = emit TokSqOpen p:
                                 blank xmlAny (InTag "[...]":w) (addcol 1 p) ss
xmlAny w p (']':ss)
    | "]>" `prefixes` ss  =
                 emit TokSectionClose p:  skip 3 p (']':ss) (xmlAny (tail w))
    | otherwise  =    emit TokSqClose p:  blank xmlAny (tail w) (addcol 1 p) ss
xmlAny w p ('(':ss) = emit TokBraOpen p:
                                 blank xmlAny (InTag "(...)":w) (addcol 1 p) ss
xmlAny w p (')':ss) = emit TokBraClose p: blank xmlAny (tail w) (addcol 1 p) ss
xmlAny w p ('=':ss) = emit TokEqual p:    blank xmlAny w (addcol 1 p) ss
xmlAny w p ('*':ss) = emit TokStar p:     blank xmlAny w (addcol 1 p) ss
xmlAny w p ('+':ss) = emit TokPlus p:     blank xmlAny w (addcol 1 p) ss
xmlAny w p ('?':ss) = emit TokQuery p:    blank xmlAny w (addcol 1 p) ss
xmlAny w p ('|':ss) = emit TokPipe p:     blank xmlAny w (addcol 1 p) ss
xmlAny w p ('%':ss) = emit TokPercent p:  blank xmlAny w (addcol 1 p) ss
xmlAny w p (';':ss) = emit TokSemi p:     blank xmlAny w (addcol 1 p) ss
xmlAny w p (',':ss) = emit TokComma p:    blank xmlAny w (addcol 1 p) ss
xmlAny w p ('#':ss) = emit TokHash p:     blank xmlAny w (addcol 1 p) ss
xmlAny w p ('"':ss) = emit TokQuote p:    textOrRefUntil "\"" TokQuote "" p1
                                                          p1 ss (xmlAny w)
                                             where p1 = addcol 1 p
xmlAny w p ('\'':ss) = emit TokQuote p:   textOrRefUntil "'" TokQuote "" p1
                                                          p1 ss (xmlAny w)
                                             where p1 = addcol 1 p
xmlAny w p s
    | isSpace (head s)     = blank xmlAny w p s
    | isAlphaNum (head s) || (head s)`elem`":_"
                           = xmlName p s "some kind of name" (blank xmlAny w)
    | otherwise            = lexerror ("unrecognised token: "++take 4 s) p

xmlTag w p s = xmlName p s "tagname for element in < >" (blank xmlAny w)

xmlSection = blank xmlSection0
  where
    xmlSection0 w p s
      | "CDATA["   `prefixes` s  = emit (TokSection CDATAx) p:  accum w p s 6
      | "INCLUDE"  `prefixes` s  = emit (TokSection INCLUDEx) p:    k w p s 7
      | "IGNORE"   `prefixes` s  = emit (TokSection IGNOREx) p:     k w p s 6
      | "%"        `prefixes` s  = emit TokPercent p:               k w p s 1
      | otherwise = lexerror ("expected CDATA, IGNORE, or INCLUDE, but got "
                             ++take 7 s) p
    accum w p s n =
      let p0 = addcol n p in
      textUntil "]]>" TokSectionClose "" p0 p0 (drop n s) (blank xmlAny w)
    k w p s n =
      skip n p s (xmlAny ({-InTag "<![section[ ... ]]>": -}w))

xmlSpecial w p s
    | "DOCTYPE"  `prefixes` s = emit (TokSpecial DOCTYPEx)  p: k 7
    | "ELEMENT"  `prefixes` s = emit (TokSpecial ELEMENTx)  p: k 7
    | "ATTLIST"  `prefixes` s = emit (TokSpecial ATTLISTx)  p: k 7
    | "ENTITY"   `prefixes` s = emit (TokSpecial ENTITYx)   p: k 6
    | "NOTATION" `prefixes` s = emit (TokSpecial NOTATIONx) p: k 8
    | otherwise = lexerror
                    ("expected DOCTYPE, ELEMENT, ENTITY, ATTLIST, or NOTATION,"
                    ++" but got "++take 7 s) p
  where k n = skip n p s (blank xmlAny w)

xmlName :: Posn -> [Char] -> [Char] -> (Posn->[Char]->[Token]) -> [Token]
xmlName p (s:ss) cxt k
    | isAlphaNum s || s==':' || s=='_'  = gatherName (s:[]) p (addcol 1 p) ss k
    | otherwise   = lexerror ("expected a "++cxt++", but got char "++show s) p
  where
    gatherName acc pos p [] k =
        emit (TokName (reverse acc)) pos: k p []
    --  lexerror ("unexpected EOF in name at "++show pos) p
    gatherName acc pos p (s:ss) k
        | isAlphaNum s || s `elem` ".-_:"
                      = gatherName (s:acc) pos (addcol 1 p) ss k
        | otherwise   = emit (TokName (reverse acc)) pos: k p (s:ss)
xmlName p [] cxt _ = lexerror ("expected a "++cxt++", but got end of input") p

xmlContent :: [Char] -> [Where] -> Posn -> Posn -> [Char] -> [Token]
xmlContent acc _w _pos p [] = if all isSpace acc then []
                            else lexerror "unexpected EOF between tags" p
xmlContent acc  w  pos p (s:ss)
    | elem s "<&"    = {- if all isSpace acc then xmlAny w p (s:ss) else -}
                       emit (TokFreeText (reverse acc)) pos: xmlAny w p (s:ss)
    | isSpace s      = xmlContent (s:acc) w pos (white s p) ss
    | otherwise      = xmlContent (s:acc) w pos (addcol 1 p) ss



--ident :: (String->TokenT) ->
--          Posn -> String -> [String] ->
--         (Posn->String->[String]->[Token]) -> [Token]
--ident tok p s ss k =
--    let (name,s0) = span (\c-> isAlphaNum c || c `elem` "`-_#.'/\\") s
--    in emit (tok name) p: skip (length name) p s ss k
