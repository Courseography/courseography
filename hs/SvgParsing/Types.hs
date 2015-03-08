module SvgParsing.Types where

data Style =
    Style {
            transform :: (Double, Double),
            fill :: String,
            stroke :: String
          } deriving Show