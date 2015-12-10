-- FpMLToHaskell
module Main where

-- This program is designed to convert a bunch of XML files containing XSD
-- module decls into a bunch of Haskell modules containing data/newtype
-- definitions which mirror the XSD.  Once you have used this program
-- to generate your type definitions, you should import Text.XML.HaXml.Schema
-- (as well as the generated modules) wherever you intend to read and write
-- XML files with your Haskell programs.

import System.Exit
import System.Environment
import System.IO
import Control.Monad
import Control.Exception as E
import System.Directory
import Data.List
import Data.Maybe (fromMaybe,catMaybes)
import Data.Function (on)
import Data.Monoid (mconcat)
--import Either

import Text.XML.HaXml            (version)
import Text.XML.HaXml.Types
import Text.XML.HaXml.Namespaces (resolveAllNames,qualify
                                 ,nullNamespace)
import Text.XML.HaXml.Parse      (xmlParse')
import Text.XML.HaXml.Util       (docContent)
import Text.XML.HaXml.Posn       (posInNewCxt)

import Text.XML.HaXml.Schema.Parse
import Text.XML.HaXml.Schema.XSDTypeModel (Schema)
import Text.XML.HaXml.Schema.NameConversion
import Text.XML.HaXml.Schema.Environment    as Env
import Text.XML.HaXml.Schema.TypeConversion as XsdToH
import Text.XML.HaXml.Schema.PrettyHaskell
import qualified Text.XML.HaXml.Schema.PrettyHsBoot     as HsBoot
import qualified Text.XML.HaXml.Schema.HaskellTypeModel as Haskell
import Text.ParserCombinators.Poly
import Text.PrettyPrint.HughesPJ (render,vcat)

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

-- sucked in from Text.XML.HaXml.Wrappers to avoid dependency on T.X.H.Html
argDirsToFiles :: IO (FilePath,[(FilePath,FilePath)])
argDirsToFiles = do
  args <- getArgs
  when ("--version" `elem` args) $ do
      putStrLn $ "part of HaXml-"++version
      exitWith ExitSuccess
  when ("--help" `elem` args) $ do
      putStrLn $ "Usage: FpMLToHaskell xsdDir haskellDir"
      putStrLn $ "    -- The results go into haskelldir/Data/FpML/file0.hs etc"
      putStrLn $ "See http://haskell.org/HaXml"
      exitWith ExitSuccess
  case args of
    [xsddir,hdir]-> do
            files <- fmap (filter (".xsd" `isSuffixOf`))
                          (getDirectoryContents xsddir)
            let newdirs = map (\file->hdir++"/"++dirOf (fpml file)) files
            mapM_ (\newdir -> do createDirectoryIfMissing True newdir) newdirs
            return (xsddir
                   ,map (\f-> (f, hdir++"/"++(reslash (fpml f))++".hs")) files)
    _ -> do prog <- getProgName
            putStrLn ("Usage: "++prog++" xsdDir haskellDir")
            exitFailure
 where
  reslash = map (\c-> case c of '.'->'/'; _->c)
  dirOf   = concat . intersperse "/" . init . wordsBy '.'
  wordsBy c s = let (a,b) = span (/=c) s in
                if null b then [a] else a: wordsBy c (tail b)

main ::IO ()
main = do
    (dir,files) <- argDirsToFiles
    deps <- flip mapM files (\ (inf,outf)-> do
        hPutStrLn stdout $ "Reading "++inf
        thiscontent <- readFileUTF8 (dir++"/"++inf)
        let d@Document{} = resolveAllNames qualify
                           . either (error . ("not XML:\n"++)) id
                           . xmlParse' inf
                           $ thiscontent
        case runParser schema [docContent (posInNewCxt inf Nothing) d] of
            (Left msg,_) -> do hPutStrLn stderr msg
                               return ([], undefined)
            (Right v,[]) ->    return (Env.gatherImports v, v)
            (Right v,_)  -> do hPutStrLn stdout $ "Parse incomplete!"
                               hPutStrLn stdout $ inf
                               hPutStrLn stdout $ "\n-----------------\n"
                               hPutStrLn stdout $ show v
                               hPutStrLn stdout $ "\n-----------------\n"
                               return ([],v)
        )
    let filedeps :: [[((FilePath,FilePath),([(FilePath,Maybe String)],Schema))]]
        filedeps  = ordered (\ ((inf,_),_)-> inf)
                            (\ (_,(ds,_))-> map fst ds)
                            (\x-> lookupWith (fst.fst) x (zip files deps))
                            (zip files deps)
        -- a single supertype environment, closed over all modules
        supertypeEnv :: Environment
        supertypeEnv = foldr (\fs e->
                              foldr (\((inf,_),(_,v))-> mkEnvironment inf v)
                                    e fs)
                             emptyEnv filedeps
        adjust :: Environment -> Environment
        adjust env = env{ env_extendty = env_extendty supertypeEnv
                        , env_substGrp = env_substGrp supertypeEnv
                        , env_allTypes = env_allTypes supertypeEnv }
        -- each module's env includes only dependencies, apart from supertypes
        environs :: [(FilePath,(Environment,FilePath,Schema))]
        environs  = flip concatMap filedeps $ \scc->
                      case scc of
                        [((inf,outf),(ds,v))]->
                          [(inf, ( adjust $ mkEnvironment inf v
                                     (foldr combineEnv emptyEnv
                                         (flip map ds
                                               (\d-> fst3 $
                                                     fromMaybe (error "FME") $
                                                     lookup (fst d) environs)
                                         )
                                     )
                                 , outf
                                 , v
                                 )
                          )]
                        cyclic ->
                            let jointSchema :: Schema
                                jointSchema = mconcat (map (snd.snd) cyclic)
                                jointDeps :: [FilePath]
                                jointDeps = concatMap (map fst.fst.snd) cyclic
                                jointEnv :: Environment
                                jointEnv = mkEnvironment "" jointSchema $
                                           foldr combineEnv emptyEnv $
                                           flip map (nub jointDeps
                                                    \\ map (fst.fst) cyclic)
                                               (\d-> fst3 $
                                                     fromMaybe (error "FME") $
                                                     lookup d environs)
                            in flip map cyclic
                                    (\((inf,outf),(_,v))->
                                      (inf,(adjust $ mkEnvironment inf v
                                                   $ jointEnv
                                           ,outf
                                           ,v)
                                      )
                                    )
    flip mapM_ environs (\ (inf,(env,outf,v))-> do
        o  <- openFile outf WriteMode
        hb <- openFile (bootf outf) WriteMode
        hSetEncoding o  utf8
        hSetEncoding hb utf8
        let decls   = XsdToH.convert env (XsdToH.typeLift v)
            haskell = Haskell.mkModule inf v decls
            doc     = ppModule fpmlNameConverter haskell
            docboot = HsBoot.ppModule fpmlNameConverter haskell
        hPutStrLn stdout $ "Writing "++outf
        hPutStrLn o $ render doc
        hPutStrLn stdout $ "Writing "++(bootf outf)
        hPutStrLn hb $ render docboot
        hFlush o
        hFlush hb
        )

-- | Munge filename for hs-boot.
bootf :: FilePath -> FilePath
bootf x = case reverse x of
            's':'h':'.':f -> reverse f++".hs-boot"
            _ -> error "bad stuff made my cheese boots melt"

-- | Calculate dependency ordering of modules, least dependent first.
--   Cyclic groups may occur, suitably placed in the ordering.
ordered :: (Eq a, Eq b) => (b->a) -> (b->[a]) -> (a->Maybe b) -> [b] -> [[b]]
ordered name deps env list =
    let cycles    = cyclicDeps name deps env list
        noncyclic = map (:[]) $ list \\ concat cycles
        workqueue = noncyclic++cycles
    in traverse [] workqueue
  where
    traverse acc []     = acc
    traverse acc (w:wq) = if all (`elem` concatMap (map name) acc)
                                 (concatMap deps w \\ map name w)
                          then traverse (acc++[w]) wq
                          else traverse     acc   (wq++[w])

-- | Find cyclic dependencies between modules.
cyclicDeps :: Eq a => (b->a) -> (b->[a]) -> (a->Maybe b) -> [b] -> [[b]]
cyclicDeps name deps env = nubBy (setEq`on`map name)
                           . (\cs-> foldl minimal cs cs)
                           . concatMap (walk [])
  where
--  walk :: [b] -> b -> [[b]]
    walk acc t = if name t `elem` map name acc then [acc]
                 else concatMap (walk (t:acc)) (catMaybes . map env $ deps t)
    minimal acc c = concatMap (prune c) acc
    prune c c' = if map name c `isProperSubsetOf` map name c' then [] else [c']
    isSubsetOf a b = all (`elem`b) a
    setEq a b            = a`isSubsetOf`b &&      b`isSubsetOf`a
    isProperSubsetOf a b = a`isSubsetOf`b && not (b`isSubsetOf`a)

-- | A variation on the standard lookup function.
lookupWith :: Eq a => (b->a) -> a -> [b] -> Maybe b
lookupWith proj x [] = Nothing
lookupWith proj x (y:ys) | proj y == x = Just y
                         | otherwise   = lookupWith proj x ys

-- | What is the targetNamespace of the unique top-level element?
targetNamespace :: Element i -> String
targetNamespace (Elem qn attrs _) =
    if qn /= xsdSchema then "ERROR! top element not an xsd:schema tag"
    else case lookup (N "targetNamespace") attrs of
           Nothing -> "ERROR! no targetNamespace specified"
           Just atv -> show atv

-- | The XSD Namespace.
xsdSchema :: QName
xsdSchema = QN (nullNamespace{nsURI="http://www.w3.org/2001/XMLSchema"})
               "schema"

-- | UTF8-clean readFile; avoids handle-leaks.
readFileUTF8 :: FilePath -> IO String
readFileUTF8 file = do
    h <- openFile file ReadMode
    (do hSetEncoding h utf8
        hGetContents h) `E.onException` (hClose h)
