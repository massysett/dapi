{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Main where

import Data.Functor.Identity (Identity)
import Control.Applicative
import Data.Char (toLower)
import Data.List (isPrefixOf)
import qualified Text.Parsec as P

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
  

newtype Lower = Lower { unLower :: String }
  deriving (Show, Eq)

instance Monad m => P.Stream Lower m Char where
  uncons (Lower x) = return $ case x of
    [] -> Nothing
    c:xs -> Just (toLower c, Lower xs)

type Parser = P.ParsecT Lower () Identity

main :: IO ()
main = undefined

matchAbbrev :: [(String, a)] -> String -> Maybe a
matchAbbrev ls s = case lookup s ls of
  Just k -> Just k
  Nothing -> case filter ((s `isPrefixOf`) . fst) ls of
    (_, v):[] -> Just v
    _ -> Nothing

data RangeSpec
  = Week
  | Month
  | Year
  | Decade
  | Century
  | Millennium
  | Quarter
  deriving (Eq, Show)

data ModText
  = This
  | Next
  | Last
  deriving (Eq, Show)

data Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
  deriving (Eq, Show)

data Sign
  = Plus
  | Minus
  deriving (Eq, Show)

data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep
  | Oct | Nov | Dec
  deriving (Eq, Show)

data RelDay = Today | Yesterday | Tomorrow
  deriving (Eq, Show)

data ModArith = ModArith Sign [Digit]
  deriving (Eq, Show)

data Mod = Mod (Either ModText ModArith)
  deriving (Eq, Show)

data Range = Range Mod RangeSpec
  deriving (Eq, Show)

data Absolute = Absolute AbsYear Month DayOrLast
  deriving (Eq, Show)

data AbsYear = AbsYear [Digit]
  deriving (Eq, Show)

data DayOrLast = DayOrLast (Either Day LastDay)
  deriving (Eq, Show)

data LastDay = LastDay
  deriving (Eq, Show)

data Day = Day [Digit]
  deriving (Eq, Show)

pRangeSpec :: Parser RangeSpec
pRangeSpec = undefined
