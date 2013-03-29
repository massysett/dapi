{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Main where

import Data.Char (toLower)
import qualified Text.Parsec as P
import qualified Data.Text as X

help :: String -> String
help pn = unlines
  [ "usage: " ++ pn ++ " [options] FIRST_DATE LAST_DATE"
  , " or " ++ pn ++ " [options] RANGE"
  , ""
  , "Options:"
  , ""
  , "--current, -c DATE_SPEC"
  , "  Set current date, used for relative dates"
  , "--quarter-start, -q MONTH"
  , "  The first day of quarter 1 is the first day of specified month"
  , "--week-start digit"
  , "  The first day of the week is the specified day"
  , "--format, -f FORMAT_SRING"
  , "  Formats dates, see strftime(3)"
  , "--base0"
  , "  Centuries, decades, and millennia begin with years that end in 0"
  , "--base1"
  , "  Centuries, decades, and millennia begin with years that end in 1"
  , "  A decade begins with a year ending in this digit"
  , ""
  , "--infix - use infix operators"
  , "--rpn - use RPN operators"
  , ""
  , "Predicates:"
  , "  relative dates are interpreted relative to the current date"
  , ""
  , "--date COMPARER DATE_SPEC"
  , "  date falls in this range"
  , "--weekday COMPARER DAY_OF_WEEK"
  , "  day of week falls in this range"
  , "--day COMPARER DAY_OR_LAST"
  , "  day falls in given range"
  , "--year COMPARER YEAR"
  , "  year falls in given range"
  , "--month COMPARER MONTH"
  , "  month falls in given range"
  , "--ends"
  , "  first or last day"
  , "--first"
  , "  First day"
  , "--last"
  , "  Last day (may predate first day)"
  , "--nth DIGITS"
  , "  every nth day (index of day modulus the given number"
  , "  equals zero)"
  , ""
  , "Operators"
  , "--and, --or, --not"
  , "--open, --close - open or close parenthesis"
  , "  (error with RPN)"
  ]
  

newtype Lower = Lower { unLower :: X.Text }
  deriving (Show, Eq)

instance Monad m => P.Stream Lower m Char where
  uncons (Lower x) = return $ case X.uncons x of
    Nothing -> Nothing
    Just (c, r) -> Just (toLower c, Lower r)

main :: IO ()
main = undefined

matchAbbrev :: [(String, a)] -> Maybe a
matchAbbrev = undefined
