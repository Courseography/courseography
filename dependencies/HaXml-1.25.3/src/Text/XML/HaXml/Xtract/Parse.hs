-- | A parser for the Xtract command-language.  (The string input is
--   tokenised internally by the lexer 'lexXtract'.)
--   See <http://www.haskell.org/HaXml/Xtract.html> for the grammar that
--   is accepted.

--   Because the original Xtract grammar was left-recursive, we have
--   transformed it into a non-left-recursive form.
module Text.XML.HaXml.Xtract.Parse (parseXtract,xtract) where

import Text.ParserCombinators.Poly hiding (bracket)
import Text.XML.HaXml.Xtract.Lex
import Text.XML.HaXml.Xtract.Combinators as D
import Text.XML.HaXml.Combinators as C
import Text.XML.HaXml.Types (Content)
import Data.List(isPrefixOf)
import Text.XML.HaXml.Escape (xmlUnEscapeContent,stdXmlEscaper)

-- output transformer - to ensure that text/references are glued together
unescape :: [Content i] -> [Content i]
unescape = xmlUnEscapeContent stdXmlEscaper


-- | To convert an Xtract query into an ordinary HaXml combinator expression.
--   First arg is a tag-transformation function (e.g. map toLower) applied
---  before matching.  Second arg is the query string.
xtract :: (String->String) -> String -> CFilter i
xtract f query
    | interiorRef lexedQ = dfilter (parseXtract lexedQ)
    | otherwise          = cfilter (parseXtract lexedQ)
  where
    lexedQ = lexXtract f query
    -- test whether query has interior reference to doc root
    interiorRef (Right (_,Symbol s): Right (_,Symbol "//"): _)
                                          | s `elem` predicateIntro = True
    interiorRef (Right (_,Symbol s): Right (_,Symbol "/"): _)
                                          | s `elem` predicateIntro = True
    interiorRef (_ : rest) = interiorRef rest
    interiorRef [] = False
    predicateIntro = [ "[", "("
                     ,  "&",   "|",  "~"
                     ,  "=",  "!=",  "<",  "<=",  ">",  ">="
                     , ".=.",".!=.",".<.",".<=.",".>.",".>=." ]

-- | The cool thing is that the Xtract command parser directly builds
--   a higher-order 'DFilter' (see "Text.XML.HaXml.Xtract.Combinators")
--   which can be applied to an XML document without further ado.
--   (@parseXtract@ halts the program if a parse error is found.)
parseXtract :: [Token] -> DFilter i
parseXtract = either error id . parseXtract'

-- | @parseXtract'@ returns error messages through the Either type.
parseXtract' :: [Token] -> Either String (DFilter i)
parseXtract' = fst . runParser (aquery liftLocal)

---- Auxiliary Parsing Functions ----
type XParser a = Parser (Either String (Posn,TokenT)) a

string :: XParser String
string = P (\inp -> case inp of
                (Left err: _) -> Failure inp err
                (Right (_,TokString n):ts) -> Success ts n
                ts -> Failure ts "expected a string" )
number :: XParser Integer
number = P (\inp -> case inp of
                (Left err: _) -> Failure inp err
                (Right (_,TokNum n):ts) -> Success ts n
                ts -> Failure ts "expected a number" )
symbol :: String -> XParser ()
symbol s = P (\inp -> case inp of
                (Left err: _) -> Failure inp err
                (Right (_, Symbol n):ts) | n==s -> Success ts ()
                ts -> Failure ts ("expected symbol "++s) )

quote :: XParser ()
quote = oneOf [ symbol "'",  symbol "\"" ]

pam :: [a->b] -> a -> [b]
pam fs x = [ f x | f <- fs ]


{--- original Xtract grammar ----
      query     = string			tagname
                | string *			tagname prefix
                | * string			tagname suffix
                | *				any element
                | -				chardata
                | ( query )
                | query / query			parent/child relationship
                | query // query		deep inside
                | query + query			union of queries
                | query [predicate]
                | query [positions]

      predicate = quattr			has attribute
                | quattr op ' string '		attribute has value
                | quattr op " string "		attribute has value
                | quattr op  quattr		attribute value comparison (lexical)
                | quattr nop integer  		attribute has value (numerical)
                | quattr nop quattr		attribute value comparison (numerical)
                | ( predicate )			bracketting
                | predicate & predicate		logical and
                | predicate | predicate		logical or
                | ~ predicate			logical not

      attribute = @ string			has attribute
                | query / @ string		child has attribute
                | -				has textual content
                | query / -			child has textual content

      quattr    = query
                | attribute

      op        =  =				equal to
                |  !=				not equal to
                |  <				less than
                |  <=				less than or equal to
                |  >				greater than
                |  >=				greater than or equal to

      nop       =  .=.				equal to
                |  .!=.				not equal to
                |  .<.				less than
                |  .<=.				less than or equal to
                |  .>.				greater than
                |  .>=.				greater than or equal to

      positions = position {, positions}	multiple positions
                | position - position		ranges

      position  = integer			numbering is from 0 upwards
                | $				last


---- transformed grammar (removing left recursion)
      aquery = ./ tquery	-- current context
             | tquery		-- also current context
             | / tquery		-- root context
             | // tquery	-- deep context from root

      tquery = ( tquery ) xquery
             | tag xquery
             | -		-- fixes original grammar ("-/*" is incorrect)
      
      tag    = string *
             | string
             | * string
             | *
      
      xquery = / tquery
             | // tquery
             | / @ string	-- new: print attribute value
             | + tquery
             | [ tpredicate ] xquery
             | [ positions ] xquery
             | lambda

      tpredicate = vpredicate upredicate
      upredicate = & tpredicate
                 | | tpredicate
                 | lambda
      vpredicate = ( tpredicate )
                 | ~ tpredicate
                 | tattribute

      tattribute = aquery uattribute
                 | @ string vattribute
      uattribute = / @ string vattribute
                 | vattribute
      vattribute = op wattribute
                 | op ' string '
                 | nop wattribute
                 | nop integer
                 | lambda
      wattribute = @ string
                 | aquery / @ string
                 | aquery

      positions  = simplepos commapos
      simplepos  = integer range
                 | $
      range      = - integer
                 | - $
                 | lambda
      commapos   = , simplepos commapos
                 | lambda

      op         =  =
                 |  !=
                 |  <
                 |  <=
                 |  >
                 |  >=

      nop        =  .=.
                 |  .!=.
                 |  .<.
                 |  .<=.
                 |  .>.
                 |  .>=.
-}

bracket :: XParser a -> XParser a
bracket p =
  do symbol "("
     x <- p
     symbol ")"
     return x


---- Xtract parsers ----

-- aquery chooses to search from the root, or only in local context
aquery ::  ((CFilter i->CFilter i) -> (DFilter i->DFilter i))
           -> XParser (DFilter i)
aquery lift = oneOf
    [ do symbol "//"
         tquery [lift C.multi]
    , do symbol "/"
         tquery [lift id]
    , do symbol "./"
         tquery [(local C.keep D./>)]
    , do tquery [(local C.keep D./>)]
    ]

tquery :: [DFilter i->DFilter i] -> XParser (DFilter i)
tquery [] = tquery [id]
tquery (qf:cxt) = oneOf
    [ do q <- bracket (tquery (qf:qf:cxt))
         xquery cxt q
    , do q <- xtag
         xquery cxt (qf ((unescape .).q))	-- glue inners texts together
    , do symbol "-"
         return (qf (local C.txt))
    ]

xtag :: XParser (DFilter i)
xtag = oneOf
    [ do s <- string
         symbol "*"
         return (local (C.tagWith (s `isPrefixOf`)))
    , do s <- string
         return (local (C.tag s))
    , do symbol "*"
         s <- string
         return (local (C.tagWith (((reverse s) `isPrefixOf`) . reverse)))
    , do symbol "*"
         return (local C.elm)
    ]


xquery :: [DFilter i->DFilter i] -> DFilter i -> XParser (DFilter i)
xquery cxt q1 = oneOf
    [ do symbol "/"
         ( do symbol "@"
              attr <- string
              return (D.iffind attr (\s->local (C.literal s)) D.none `D.o` q1)
           `onFail`
           tquery ((q1 D./>):cxt) )
    , do symbol "//"
         tquery ((\q2-> (liftLocal C.multi) q2
                            `D.o` local C.children `D.o` q1):cxt)
    , do symbol "+"
         q2 <- tquery cxt
         return (D.cat [q1,q2])
    , do symbol "["
         is <- iindex	-- now extended to multiple indexes
         symbol "]"
         xquery cxt (\xml-> concat . pam is . q1 xml)
    , do symbol "["
         p <- tpredicate
         symbol "]"
         xquery cxt (q1 `D.with` p)
    , return q1
    ]

tpredicate :: XParser (DFilter i)
tpredicate =
  do p <- vpredicate
     f <- upredicate
     return (f p)

upredicate :: XParser (DFilter i->DFilter i)
upredicate = oneOf
    [ do symbol "&"
         p2 <- tpredicate
         return (`D.o` p2)
    , do symbol "|"
         p2 <- tpredicate
         return (D.|>| p2)
    , return id
    ]

vpredicate :: XParser (DFilter i)
vpredicate = oneOf
    [ do bracket tpredicate
    , do symbol "~"
         p <- tpredicate
         return (local C.keep `D.without` p)
    , do tattribute
    ]

tattribute :: XParser (DFilter i)
tattribute = oneOf
    [ do q <- aquery liftGlobal
         uattribute q
    , do symbol "@"
         s <- string
         vattribute (local C.keep, local (C.attr s), D.iffind s)
    ]

uattribute :: DFilter i -> XParser (DFilter i)
uattribute q = oneOf
    [ do symbol "/"
         symbol "@"
         s <- string
         vattribute (q, local (C.attr s), D.iffind s)
    , do vattribute (q, local C.keep,     D.ifTxt)
    ]

vattribute :: (DFilter i, DFilter i, (String->DFilter i)->DFilter i->DFilter i)
              -> XParser (DFilter i)
vattribute (q,a,iffn) = oneOf
  [ do cmp <- op
       quote
       s2 <- string
       quote
       return ((iffn (\s1->if cmp s1 s2 then D.keep else D.none) D.none)
               `D.o` q)
  , do cmp <- op
       (q2,iffn2) <- wattribute	-- q2 unused?  is this a mistake?
       return ((iffn (\s1-> iffn2 (\s2-> if cmp s1 s2 then D.keep else D.none)
                                  D.none)
                     D.none) `D.o` q)
  , do cmp <- nop
       n <- number
       return ((iffn (\s->if cmp (read s) n then D.keep else D.none) D.none)
               `D.o` q)
  , do cmp <- nop
       (q2,iffn2) <- wattribute	-- q2 unused?  is this a mistake?
       return ((iffn (\s1-> iffn2 (\s2-> if cmp (read s1) (read s2) then D.keep
                                                                    else D.none)
                                  D.none)
                     D.none) `D.o` q)
  , do return ((a `D.o` q))
  ]

wattribute :: XParser (DFilter i, (String->DFilter i)->DFilter i->DFilter i)
wattribute = oneOf
    [ do symbol "@"
         s <- string
         return (D.keep, D.iffind s)
    , do q <- aquery liftGlobal
         symbol "/"
         symbol "@"
         s <- string
         return (q, D.iffind s)
    , do q <- aquery liftGlobal
         return (q, D.ifTxt)
    ]


iindex :: XParser [[a]->[a]]
iindex =
    do i <- simpleindex
       is <- idxcomma
       return (i:is)

simpleindex :: XParser ([a]->[a])
simpleindex = oneOf
    [ do n <- number
         r <- rrange n
         return r
    , do symbol "$"
         return (C.keep . last)
    ]

rrange, numberdollar :: Integer -> XParser ([a]->[a])
rrange n1 = oneOf
    [ do symbol "-"
         numberdollar n1
    , return (take 1 . drop (fromInteger n1))
    ]

numberdollar n1 = oneOf
    [ do n2 <- number
         return (take (fromInteger (1+n2-n1)) . drop (fromInteger n1))
    , do symbol "$"
         return (drop (fromInteger n1))
    ]

idxcomma :: XParser [[a]->[a]]
idxcomma = oneOf
    [ do symbol ","
         r <- simpleindex
         rs <- idxcomma
         return (r:rs)
    , return []
    ]


op :: XParser (String->String->Bool)
op = oneOf
    [ do symbol "=";  return (==)
    , do symbol "!="; return (/=)
    , do symbol "<";  return (<)
    , do symbol "<="; return (<=)
    , do symbol ">";  return (>)
    , do symbol ">="; return (>=)
    ]

nop :: XParser (Integer->Integer->Bool)
nop = oneOf
    [ do symbol ".=.";  return (==)
    , do symbol ".!=."; return (/=)
    , do symbol ".<.";  return (<)
    , do symbol ".<=."; return (<=)
    , do symbol ".>.";  return (>)
    , do symbol ".>=."; return (>=)
    ]

