-- | A streaming XML parser, using a method known as SAX. SAX isn't really a
--   standard, but an implementation, so it's just an \"SAX-like\" parser.
--   This module allows you parse an XML document without having to evaluate
--   it as a whole. This is needed for protocols like jabber, which use xml
--   streams for communication.

module Text.XML.HaXml.SAX
	( SaxElement(..)
	, saxParse
	) where

import Text.XML.HaXml.Types
import Text.XML.HaXml.Parse
import Text.XML.HaXml.Lex
import Text.ParserCombinators.Poly.State

data SaxElement
	= SaxDocTypeDecl DocTypeDecl
		-- ^ A doctype declaration occured(\<!DOCTYPE\>)
	| SaxProcessingInstruction ProcessingInstruction
		-- ^ A processing instruction occured (\<??\>)
	| SaxComment String		-- ^ A comment occured (\<!-- --\>)
	| SaxElementOpen Name [Attribute] -- ^ An element was opened (\<\>)
	| SaxElementClose Name		-- ^ An element was closed (\<\/\>)
	| SaxElementTag Name [Attribute]
		-- ^ An element without content occured (\<\/\>)
	| SaxCharData CharData		-- ^ Some string data occured
	| SaxReference Reference	-- ^ A reference occured

-- | @saxParse file content@ takes a filename and the string content of that
--   file and generates a stream of @SaxElement@s. If an error occurs, the
--   parsing stops and a string is returned using the @Maybe@ type.
saxParse :: String -- ^ The filename
	 -> String -- ^ The content of the file
	 -> ([SaxElement],Maybe String)
		-- ^ A tuple of the parsed elements and @Nothing@, if no
		--   error occured, or @Just@ @String@ if an error occured.
saxParse file cntnt = parseStream sax emptySTs
                                    (xmlLex file cntnt)

parseStream :: Parser s t a -> s -> [t] -> ([a], Maybe String)
parseStream _ _ [] = ([],Nothing)
parseStream p state toks = case runParser p state toks of
	(Left err, _, _) -> ([],Just err)
	(Right res, nstate, rest) -> (res:moreres, err)
            where (moreres,err) = parseStream p nstate rest

sax :: XParser SaxElement
sax = oneOf [ saxelementopen
            , saxelementclose
            , saxprocessinginstruction
            , saxcomment
            , saxdoctypedecl
            , saxreference
            , saxchardata
            ]
	`adjustErr` (++("\nLooking for a SAX event:\n"
               ++"  elem-open, elem-close, PI, comment, DTD, ref, or chardata"))

saxelementopen :: XParser SaxElement
saxelementopen = do
	tok TokAnyOpen
	(ElemTag (N n) as) <- elemtag  -- no QN ever generated during parsing
	(( do tok TokEndClose
	      return (SaxElementTag n as)) `onFail`
	 ( do tok TokAnyClose
	      return (SaxElementOpen n as))
	 `onFail` fail "missing > or /> in element tag")

saxelementclose :: XParser SaxElement
saxelementclose = do
	tok TokEndOpen
	n <- name
	tok TokAnyClose
	return (SaxElementClose n)

saxcomment :: XParser SaxElement
saxcomment = comment >>= return . SaxComment

saxchardata :: XParser SaxElement
saxchardata =
  (cdsect >>= return . SaxCharData)
  `onFail`
  (chardata >>= return . SaxCharData)

saxreference :: XParser SaxElement
saxreference = reference >>= return . SaxReference

saxdoctypedecl :: XParser SaxElement
saxdoctypedecl = doctypedecl >>= return . SaxDocTypeDecl

saxprocessinginstruction :: XParser SaxElement
saxprocessinginstruction = fmap SaxProcessingInstruction processinginstruction
