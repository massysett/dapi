{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module Main where

import Control.Applicative
  ( many, (<$), (<|>), some, (<$>),
    (<*>), (<*))

import Control.Monad (guard)
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Time as T
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Functor.Identity (Identity)
import Data.Char (toLower)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe)
import Text.Parsec (char)
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

data DateSpec = DateSpec (Either Absolute Relative)
  deriving (Eq, Show)

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

data RelRange = RelRange ModArith RangeSpec
  deriving (Eq, Show)

data DayOfWeek = Sun | Mon | Tue | Wed | Thu | Fri | Sat
  deriving (Eq, Show)

data Relative = Relative (Either RelDay RelRange)
  deriving (Eq, Show)

pRangeSpec :: Parser RangeSpec
pRangeSpec = parseList "range specification"
  [ ("week", Week), ("month", Month), ("year", Year)
  , ("decade", Decade), ("century", Century)
  , ("millennium", Millennium), ("quarter", Quarter) ]

pModText :: Parser ModText
pModText = parseList "modifier"
  [ ("this", This), ("next", Next), ("last", Last) ]

pSign :: Parser Sign
pSign = Plus <$ char '+' <|> Minus <$ char '-'

spaces :: Parser ()
spaces = () <$ many (char ' ')

pDigit :: Parser Digit
pDigit = f <$> P.digit
  where
    f d = case d of
      { '0' -> D0; '1' -> D1; '2' -> D2; '3' -> D3;
        '4' -> D4; '5' -> D5; '6' -> D6; '7' -> D7;
        '8' -> D8; '9' -> D9;
        _ -> error "pDigit: error: Parsec digit failed" }

pModArith :: Parser ModArith
pModArith = ModArith <$> pSign <*> some pDigit

pEither :: Parser a -> Parser b -> Parser (Either a b)
pEither a b = Left <$> a <|> Right <$> b

pMod :: Parser Mod
pMod = Mod <$> pEither pModText pModArith

pRange :: Parser Range
pRange = Range <$> pMod <*> pRangeSpec

pRelDay :: Parser RelDay
pRelDay = parseList "relative day"
  [ ("today", Today), ("yesterday", Yesterday)
  , ("tomorrow", Tomorrow) ]

pAbsolute :: Parser Absolute
pAbsolute = Absolute <$> pAbsYear <* separator <*> pMonth
            <* separator <*> pDayOrLast

separator :: Parser ()
separator = () <$ (char '-' <|> char '/')

parseList
  :: String
  -- ^ Description of what we are parsing, for error message
  -> [(String, a)]
  -> Parser a

parseList s ls = do
  str <- some P.letter
  maybe (fail $ "could not parse " ++ s ++ ": " ++ str)
    return $ matchAbbrev ls str


pMonthAbbrev :: Parser Month
pMonthAbbrev = parseList "month abbreviation"
  [ ("jan", Jan), ("feb", Feb), ("mar", Mar), ("apr", Apr)
  , ("may", May), ("jun", Jun), ("jul", Jul), ("aug", Aug)
  , ("sep", Sep), ("oct", Oct), ("nov", Nov), ("dec", Dec) ]

pMonthFromDigits :: Parser Month
pMonthFromDigits = some pDigit >>= parseDigits
  where
    parseDigits ls = case ls of
      [] -> error "pMonthFromDigits: some returned an empty list"
      d1:[] -> fromOneDigit d1
      d1:d2:[] -> case d1 of
        D0 -> fromOneDigit d2
        D1 -> case d2 of
          D0 -> return Oct
          D1 -> return Nov
          D2 -> return Dec
          _ -> err
        _ -> err
      _ -> err
      where
        err = fail $ "invalid month: " ++ map digitChar ls
        fromOneDigit d = case d of
          { D0 -> err;

            D1 -> return Jan;  D2 -> return Feb;  D3 -> return Mar;
            D4 -> return Apr;  D5 -> return May;  D6 -> return Jun;
            D7 -> return Jul;  D8 -> return Aug;  D9 -> return Sep }

digitChar :: Digit -> Char
digitChar d = case d of
  { D0 -> '0'; D1 -> '1'; D2 -> '2'; D3 -> '3'; D4 -> '4';
    D5 -> '5'; D6 -> '6'; D7 -> '7'; D8 -> '8'; D9 -> '9' }

monthToInt :: Month -> Int
monthToInt m = case m of
  { Jan -> 1; Feb -> 2; Mar -> 3; Apr -> 4; May -> 5; Jun -> 6;
    Jul -> 7; Aug -> 8; Sep -> 9; Oct -> 10; Nov -> 11; Dec -> 12 }

pAbsYear :: Parser AbsYear
pAbsYear = AbsYear <$> some pDigit

pMonth :: Parser Month
pMonth = pMonthFromDigits <|> pMonthAbbrev

pDay :: Parser Day
pDay = Day <$> some pDigit

pLastDay :: Parser LastDay
pLastDay = LastDay <$ char 'l'

pDayOrLast :: Parser DayOrLast
pDayOrLast = DayOrLast <$> pEither pDay pLastDay

pRelative :: Parser Relative
pRelative = Relative <$> pEither pRelDay pRelRange

pRelRange :: Parser RelRange
pRelRange = RelRange <$> pModArith <* spaces <*> pRangeSpec

pDateSpec :: Parser DateSpec
pDateSpec = DateSpec <$> pEither pAbsolute pRelative

--
--
--

-- | Given a particular range, returns the lower and upper bound of
-- the indicated span that includes the given number. Calls error if
-- any of the arguments are negative.
inRange

  :: Integral i
  => BaseOne
  -> Int
  -- ^ Exponent indicating which range the number must be
  -- within. For example, 0 returns simply the number as the lower and
  -- upper result. 1 returns a range that has 10 items. 2 returns a
  -- range that has 100 items.

  -> i
  -- ^ Number we are checking

  -> (i, i)
  -- ^ Lower and upper bounds of range

inRange b e n = fromMaybe (error "inRange: negative argument") $ do
  guard $ e >= 0
  guard $ n >= 0
  let size = 10 ^ e
      rmdr = n `mod` (10 ^ e)
      (pLwr, pUpr) = (n - rmdr, (n + (size - rmdr) - 1))
  return $ if b
    then if n `mod` 10 == 0
         then let lwr' = pLwr - (size - 1)
              in (lwr', lwr' + (size - 1))
         else (pLwr + 1, pUpr + 1)
    else (pLwr, pUpr)

--
--
--

weekRange
  :: DayOfWeek
  -- ^ Week starts on this day
  -> T.Day
  -> [T.Day]
weekRange dow d = [ lwr .. upr ]
  where
    start = case dow of
      { Sun -> 7; Mon -> 1; Tue -> 2; Wed -> 3; Thu -> 4;
        Fri -> 5; Sat -> 6 }
    (_, _, wd) = toWeekDate d
    lwrAdj = if start > wd then wd + (7 - start) else wd - start
    lwr = T.addDays (negate . fromIntegral $ lwrAdj) d
    upr = T.addDays 6 lwr

monthRange :: T.Day -> [T.Day]
monthRange d = [ lwr .. upr ]
  where
    (y, m, _) = T.toGregorian d
    lwr = T.fromGregorian y m 01
    upr = T.fromGregorian y m (T.gregorianMonthLength y m)

yearRange :: T.Day -> [T.Day]
yearRange d = [ lwr .. upr ]
  where
    (y, _, _) = T.toGregorian d
    lwr = T.fromGregorian y 1 1
    upr = T.fromGregorian y 12 31

type BaseOne = Bool

baseTenRange
  :: Int
  -- ^ Exponent to use
  -> BaseOne -> T.Day -> [T.Day]
baseTenRange e b d = [ lwr .. upr ]
  where
    (y, _, _) = T.toGregorian d
    (fstYr, lstYr) = inRange b e y
    lwr = T.fromGregorian fstYr 1 1
    upr = T.fromGregorian lstYr 12 31

decadeRange :: BaseOne -> T.Day -> [T.Day]
decadeRange = baseTenRange 1

centuryRange :: BaseOne -> T.Day -> [T.Day]
centuryRange = baseTenRange 2

quarterRange
  :: T.Day
  -> [T.Day]
quarterRange d =
  let (by, bm, _) = T.toGregorian d
      (mFst, mLst) = case () of
        _ | bm < 4 -> (1, 3)
          | bm < 7 -> (4, 6)
          | bm < 10 -> (7, 9)
          | otherwise -> (10, 12)
      fstDy = T.fromGregorian by mFst 1
      lstDy = T.fromGregorian by mLst (T.gregorianMonthLength by mLst)
  in [fstDy .. lstDy]

millenniumRange :: BaseOne -> T.Day -> [T.Day]
millenniumRange = baseTenRange 3

calcRange
  :: DayOfWeek
  -> BaseOne
  -> RangeSpec
  -> T.Day
  -> [T.Day]
calcRange dow b1 r = case r of
  Week -> weekRange dow
  Month -> monthRange
  Year -> yearRange
  Decade -> decadeRange b1
  Century -> centuryRange b1
  Millennium -> millenniumRange b1
  Quarter -> quarterRange

modifyDate :: Mod -> RangeSpec -> T.Day -> T.Day
modifyDate m r d = case r of
  Week -> T.addDays (nMod m * 7) d

nMod :: Mod -> Integer
nMod m = case m of
  Left t -> case t of
    This -> 0
    Next -> 1
    Last -> (-1)
  Right (ModArith s ds)
    
