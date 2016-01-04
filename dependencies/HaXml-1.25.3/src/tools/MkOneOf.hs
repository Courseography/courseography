module Main where

import Prelude hiding (max)
import System.Exit        (exitWith,ExitCode(..))
import System.Environment (getArgs)
import Data.Char          (isDigit)
import System.IO          (hFlush,stdout)
import Control.Monad      (when)
import Text.XML.HaXml     (version)

main  :: IO ()
main = do
    args <- getArgs
    when ("--version" `elem` args) $ do
        putStrLn $ "part of HaXml-"++version
        exitWith ExitSuccess
    when ("--help" `elem` args) $ do
        putStrLn $ "See http://haskell.org/HaXml"
        exitWith ExitSuccess
    case length args of
      1 -> do n <- saferead (head args)
              putStrLn ("module Text.XML.HaXml."++constructor 1 n++" where\n")
              putStrLn ("import Text.XML.HaXml.XmlContent\n")
              putStrLn (mkOneOf n)
      2 -> do n <- saferead (args!!0)
              m <- saferead (args!!1)
              putStrLn ("module Text.XML.HaXml.OneOfN where\n")
              putStrLn ("import Text.XML.HaXml.XmlContent\n")
              mapM_ (putStrLn . mkOneOf) [n..m]
      _ -> error "Usage: MkOneOf n [m]"
    hFlush stdout

---- main text-generating function ----
mkOneOf :: Int -> String
mkOneOf n =
    "data "++ typename n 12
    ++ "\n   "++ format 3 78 3 " = " " | "
                        (zipWith (\m v->constructor m n++" "++v)
                                 [1..n]
                                 (take n variables))
    ++ "\n    deriving (Eq,Show)"
    ++ "\n\ninstance "++ format 9 78 9 "(" ","
                                (map ("HTypeable "++) (take n variables))
    ++ ")\n    => HTypeable ("++ typename n 26 ++")\n  where"
    ++ "      toHType _ = Defined \""++constructor 1 n++"\" [] []"
    ++ "\n\ninstance "++ format 9 78 9 "(" ","
                                (map ("XmlContent "++) (take n variables))
    ++ ")\n    => XmlContent ("++ typename n 26 ++")\n  where"
    ++ "\n    parseContents ="
    ++ "\n       "++ format 7 78 7 " (" " $ "
                            (map (\v->"choice "++constructor v n) [1..n])
    ++ "\n        $ fail \""++constructor 1 n++"\")"
    ++ concatMap (\v->"\n    toContents ("++constructor v n
                                          ++" x) = toContents x")
                 [1..n]
    ++ "\n\nfold"++constructor 1 n++" :: "
                 ++format 15 78 15 "" ""
                          (map (\v->"("++v++"->z) -> ") (take n variables))
                 ++"\n               "
                 ++constructor 1 n++format 22 78 22 " " " " (take n variables)
                 ++"\n               -> z"
    ++ concat (zipWith (\i v-> "\n"++"fold"++constructor 1 n
                               ++format 11 50 11 " " " " (take n variables)
                               ++" ("++constructor i n ++" z) = "++v++" z")
                       [1..n]
                       (take n variables))
    ++ "\n\n----"

---- constructor names ----
typename :: Int -> Int -> String
typename n pos = constructor 1 n ++ format pos 78 pos " " " " (take n variables)

constructor :: Int -> Int -> String
constructor n m = ordinal n ++"Of" ++ show m

ordinal :: Int -> String
ordinal n | n <= 20   = ordinals!!n
ordinal n | otherwise = "Choice"++show n

ordinals :: [String]
ordinals = ["Zero","One","Two","Three","Four","Five","Six","Seven","Eight"
           ,"Nine","Ten","Eleven","Twelve","Thirteen","Fourteen","Fifteen"
           ,"Sixteen","Seventeen","Eighteen","Nineteen","Twenty"]

---- variable names ----
variables :: [String]
variables = [ v:[] | v <- ['a'..'y']]
            ++ [ v:w:[] | v <- ['a'..'z'], w <- ['a'..'z']]

---- simple pretty-printing ----

format :: Int		-- current position on page
       -> Int		-- maximum width of page
       -> Int		-- amount to indent when a newline is emitted
       -> String	-- text to precede first value
       -> String	-- text to precede subsequent values
       -> [String]	-- list of values to format
        -> String
format _cur _max _ind _s0 _s1 []     = ""
format  cur  max  ind  s0  s1 (x:xs)
    | sameline < max  = s0 ++ x ++ format sameline max ind s1 s1 xs
    | otherwise       = "\n" ++ replicate ind ' ' ++
                        s0 ++ x ++ format newline max ind s1 s1 xs
                where sameline = cur + length s0 + length x
                      newline  = ind + length s0 + length x

---- safe integer parsing ----
saferead :: String -> IO Int
saferead s | all isDigit s = return (read s)
saferead s | otherwise     = error ("expected a number on the commandline, "
                                    ++"but got \""++s++"\" instead")
