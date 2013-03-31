{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative
  ( many, (<$), (<|>), some, (<$>),
    (<*>), (<*))

import Control.Monad (guard, (>=>), when)
import qualified Control.Monad.Exception.Synchronous as Ex
import qualified Data.Time as T
import Data.Time.Calendar.WeekDate (toWeekDate)
import Data.Functor.Identity (Identity)
import Data.Char (toLower)
import Data.Either (partitionEithers)
import Data.List (isPrefixOf, foldl', unfoldr)
import Data.Maybe (fromMaybe, catMaybes)
import qualified Data.Text as X
import Data.Text (Text)
import qualified Data.Prednote.Expressions as Exp
import qualified Data.Prednote.Pdct as Pd
import Data.Prednote.Pdct ((|||))
import Text.Parsec (char)
import qualified Text.Parsec as P
import qualified System.Console.MultiArg as MA
import qualified System.IO as IO
import qualified System.Exit as Exit
import qualified System.Console.Rainbow as R
import System.Locale (defaultTimeLocale)

------------------------------------------------------------
------------------------------------------------------------
-- BEGIN CONFIGURABLE OPTIONS
------------------------------------------------------------
------------------------------------------------------------

-- | Put the options you would like to use by default in this record.
myOpts :: Opts
myOpts = Opts
  { oWeekStart = Sun
  , oBase = False
  , oFormat = "%Y-%m-%d"
  , oExprDesc = Exp.RPN
  , oShowExpression = False
  , oVerboseFilter = False
  , oColorToFile = False
  }


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
  , "--show-expression - show filtering expression (toggle)"
  , "--verbose-filter - filter days verbosely (toggle)"
  , "--color-to-file - use colors when stdout is not a terminal"
  , "                  (toggle)"
  , ""
  , "Predicates:"
  , "  relative dates are interpreted relative to the current date"
  , ""
  , "--date, -d COMPARER DATE_SPEC"
  , "  date falls in this range"
  , "--weekday, -w COMPARER DAY_OF_WEEK"
  , "  day of week falls in this range"
  , "--day COMPARER DAY_OR_LAST"
  , "  day falls in given range"
  , "--year, y COMPARER YEAR"
  , "  year falls in given range"
  , "--month, m COMPARER MONTH"
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

matchAbbrev :: [(String, a)] -> String -> Maybe a
matchAbbrev ls s = case lookup s ls of
  Just k -> Just k
  Nothing -> case filter ((s `isPrefixOf`) . fst) ls of
    (_, v):[] -> Just v
    _ -> Nothing

matchDayOfWeek :: String -> Maybe DayOfWeek
matchDayOfWeek = matchAbbrev
  [ ("sunday", Sun), ("monday", Mon), ("tuesday", Tue)
  , ("wednesday", Wed), ("thursday", Thu)
  , ("friday", Fri), ("saturday", Sat) ]

--
-- Date types - beginning with most primitive
--

data Digit = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9
  deriving (Eq, Show)

digitToInt :: Integral i => Digit -> i
digitToInt d = case d of
  { D0 -> 0; D1 -> 1; D2 -> 2; D3 -> 3; D4 -> 4; D5 -> 5;
    D6 -> 6; D7 -> 7; D8 -> 8; D9 -> 9 }

digitsToInt :: Integral i => [Digit] -> i
digitsToInt = foldl' f 0 . zip [ (0 :: Int) ..] . reverse
  where
    f tot (places, d) = tot + ((10 ^ places) * digitToInt d)

data Day = Day [Digit]
  deriving (Eq, Show)

data Sign
  = Plus
  | Minus
  deriving (Eq, Show)

data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep
  | Oct | Nov | Dec
  deriving (Eq, Show, Ord)

data AbsYear = AbsYear [Digit]
  deriving (Eq, Show)

data LastDay = LastDay
  deriving (Eq, Show)

data DayOfWeek = Sun | Mon | Tue | Wed | Thu | Fri | Sat
  deriving (Eq, Show, Ord)

-- | The command line can contain dates or, alternatively, a single
-- range. A DateSpec represents one of the two dates if the user chose
-- to use dates.
data DateSpec = DateSpec (Either Absolute Relative)
  deriving (Eq, Show)

dayDateSpec
  :: T.Day -> DateSpec -> Maybe T.Day
dayDateSpec d (DateSpec ds) = case ds of
  Left ab -> dayAbsolute ab
  Right rel -> Just $ dayRelative d rel

--
-- Absolute dates
--

data DayOrLast = DayOrLast (Either Day LastDay)
  deriving (Eq, Show)

-- | A single absolute date. Can be derived without any relative date
-- information.
data Absolute = Absolute AbsYear Month DayOrLast
  deriving (Eq, Show)

dayAbsolute :: Absolute -> Maybe T.Day
dayAbsolute (Absolute (AbsYear yds) m (DayOrLast dl)) =
  let yr = digitsToInt yds
      mo = monthToInt m
      da = case dl of
        Left (Day d) -> digitsToInt d
        Right _ -> T.gregorianMonthLength yr mo
  in T.fromGregorianValid yr mo da

--
-- Relative dates
--
data ModArith = ModArith Sign [Digit]
  deriving (Eq, Show)

nModArith :: Integral i => ModArith -> i
nModArith (ModArith s ds) = flipSign (digitsToInt ds)
  where
    flipSign = case s of { Plus -> id; Minus -> negate }
 
data RelDay = Today | Yesterday | Tomorrow
  deriving (Eq, Show)

relDayToInt :: Integral i => RelDay -> i
relDayToInt d = case d of
  Today -> 0
  Yesterday -> (-1)
  Tomorrow -> 1

dayRelDay :: RelDay -> T.Day -> T.Day
dayRelDay r = T.addDays (relDayToInt r)

data RelRange = RelRange ModArith RangeSpec
  deriving (Eq, Show)

dayRelRange :: T.Day -> RelRange -> T.Day
dayRelRange d (RelRange m s) = modifyDate (Mod (Right m)) s d

data Relative = Relative (Either RelDay RelRange)
  deriving (Eq, Show)

dayRelative :: T.Day -> Relative -> T.Day
dayRelative d (Relative e) = case e of
  Left rd -> dayRelDay rd d
  Right rr -> dayRelRange d rr

--
-- Ranges
--

data RangeSpec
  = Week
  | Month
  | Year
  | Decade
  | Century
  | Millennium
  | Quarter
  deriving (Eq, Show)

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

rangeSpecToList
  :: DayOfWeek
  -> BaseOne
  -> RangeSpec
  -> T.Day
  -> [T.Day]
rangeSpecToList dow b1 r = case r of
  Week -> weekRange dow
  Month -> monthRange
  Year -> yearRange
  Decade -> decadeRange b1
  Century -> centuryRange b1
  Millennium -> millenniumRange b1
  Quarter -> quarterRange

data ModText
  = This
  | Next
  | Last
  deriving (Eq, Show)

data Mod = Mod (Either ModText ModArith)
  deriving (Eq, Show)

nMod :: Mod -> Integer
nMod (Mod m) = case m of
  Left t -> case t of
    This -> 0
    Next -> 1
    Last -> (-1)
  Right ma -> nModArith ma

modifyDate :: Mod -> RangeSpec -> T.Day -> T.Day
modifyDate m r d =
  let n = nMod m
  in case r of
      Week -> T.addDays (n * 7) d
      Month -> T.addGregorianMonthsClip n d
      Year -> T.addGregorianYearsClip n d
      Decade -> T.addGregorianYearsClip (n * 10) d
      Century -> T.addGregorianYearsClip (n * 100) d
      Millennium -> T.addGregorianYearsClip (n * 1000) d
      Quarter -> T.addGregorianMonthsClip (n * 3) d

data Range = Range Mod RangeSpec
  deriving (Eq, Show)

rangeToList
  :: DayOfWeek
  -> BaseOne
  -> T.Day
  -- ^ Current day
  -> Range
  -> [T.Day]
rangeToList dow b d (Range m r) = rangeSpecToList dow b r (modifyDate m r d)

--
-- Parsers
--

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
pRange = Range <$> pMod <* spaces <*> pRangeSpec

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
-- Parsing the command line
--

parseArgs
  :: DayOfWeek
  -> BaseOne
  -> T.Day
  -> [String]
  -> Ex.Exceptional String [T.Day]
parseArgs dow b1 d ss = case ss of
  [] -> Ex.throw "no dates given on command line."
  x:[] -> case P.parse (pRange <* P.eof) "" (Lower x) of
    Left e -> Ex.throw $ "could not parse range: " ++ show e
    Right r -> return $ rangeToList dow b1 d r
  x1:x2:[] -> do
    a1 <- case P.parse (pDateSpec <* P.eof) "" (Lower x1) of
      Left e -> Ex.throw $ "could not parse first date: " ++ show e
      Right r -> return r
    a2 <- case P.parse (pDateSpec <* P.eof) "" (Lower x2) of
      Left e -> Ex.throw $ "could not parse second date: " ++ show e
      Right r -> return r
    datesToList d a1 a2
  _ -> Ex.throw "too many command line arguments."

datesToList
  :: T.Day
  -> DateSpec
  -> DateSpec
  -> Ex.Exceptional String [T.Day]
datesToList b a1 a2 = do
  d1 <- Ex.fromMaybe "invalid first date given" $ dayDateSpec b a1
  d2 <- Ex.fromMaybe "invalid second date given" $ dayDateSpec d1 a2
  return $ unfoldr (calcNextDay d2) d1

calcNextDay
  :: T.Day
  -- ^ Stop iterating after this day is produced
  -> T.Day
  -- ^ Increment or decrement this day, and add the result to the list
  -> Maybe (T.Day, T.Day)
  -- ^ Nothing to stop iteration. Just (a, a) if iteration is to
  -- continue, where a is the value to add to the list as well as the
  -- next seed.

calcNextDay stop d =
  if d == stop
  then Nothing
  else let amtToAdd = if d < stop then 1 else (-1)
           nextDay = T.addDays amtToAdd d
       in Just (d, nextDay)

dayInfos :: [T.Day] -> [DayInfo]
dayInfos ds = zipWith3 f fwds baks ds
  where
    fwds = [0..]
    baks = iterate pred (length ds - 1)
    f fwd bak d = DayInfo d fwd bak

--
-- CLI
--

data DayInfo = DayInfo
  { iDay :: T.Day
  , iFwd :: Int
  , iBack :: Int
  }

data Opts = Opts
  { oWeekStart :: DayOfWeek
  , oBase :: BaseOne
  , oFormat :: String
  , oExprDesc :: Exp.ExprDesc
  , oShowExpression :: Bool
  , oVerboseFilter :: Bool
  , oColorToFile :: Bool
  }

data ParseOpts = ParseOpts
  { pWeekStart :: DayOfWeek
  , pBase :: BaseOne
  , pFormat :: String
  , pExprDesc :: Exp.ExprDesc
  , pCurrent :: T.Day
  , pOperands :: [Exp.Token DayInfo]
  , pShowExpression :: Bool
  , pVerboseFilter :: Bool
  , pColorToFile :: Bool
  }

initParseOpts :: T.Day -> Opts -> ParseOpts
initParseOpts d o = ParseOpts
  { pWeekStart = oWeekStart o
  , pBase = oBase o
  , pFormat = oFormat o
  , pExprDesc = oExprDesc o
  , pCurrent = d
  , pOperands = []
  , pShowExpression = oShowExpression o
  , pVerboseFilter = oVerboseFilter o
  , pColorToFile = oColorToFile o
  }

addOperand :: Exp.Token DayInfo
           -> ParseOpts -> ParseOpts
addOperand t o = o { pOperands = pOperands o ++ [t] }

-- | Parses a string using a Parsec parser and, if it fails, returns a
-- Multiarg error.
parsecMultiarg
  :: Parser a
  -> String
  -> Ex.Exceptional MA.OptArgError a
parsecMultiarg p s = case P.parse (p <* P.eof) "" (Lower s) of
  Left e -> Ex.throw . MA.ErrorMsg . init . show $ e
  Right g -> return g

allOpts :: [MA.OptSpec (ParseOpts -> Ex.Exceptional String ParseOpts)]
allOpts =

  [ MA.OptSpec ["current"] "c" . MA.OneArgE $ \s -> do
    ds <- parsecMultiarg pDateSpec s
    return $ \o -> do
      let baseDay = pCurrent o
      d <- Ex.fromMaybe ("invalid date: " ++ s) $ dayDateSpec baseDay ds
      return o { pCurrent = d }

  , MA.OptSpec ["week-start"] "" . MA.OneArgE $ \s ->
    case matchDayOfWeek (map toLower s) of
      Nothing -> Ex.throw . MA.ErrorMsg $ "invalid day of week"
      Just dow -> return $ \o -> return $ o { pWeekStart = dow }

  , MA.OptSpec ["format"] "f" . MA.OneArg $ \s o ->
    return o { pFormat = s }

  , MA.OptSpec ["base0"] "" (MA.NoArg (\o -> return o { pBase = False }))
  , MA.OptSpec ["base1"] "" (MA.NoArg (\o -> return o { pBase = True }))

  , MA.OptSpec ["infix"] ""
    (MA.NoArg (\o -> return o { pExprDesc = Exp.Infix }))

  , MA.OptSpec ["rpn"] ""
    (MA.NoArg (\o -> return o { pExprDesc = Exp.RPN }))

  , MA.OptSpec ["show-expression"] ""
    (MA.NoArg (\o -> return o { pShowExpression =
                                not (pShowExpression o) }))

  , MA.OptSpec ["verbose-filter"] ""
    (MA.NoArg (\o -> return o { pVerboseFilter =
                                not (pVerboseFilter o) }))

  , MA.OptSpec ["color-to-file"] ""
    (MA.NoArg (\o -> return o { pColorToFile =
                                not (pColorToFile o) }))

  , pdctDate
  , pdctWeekday
  , pdctDay
  , pdctYear
  , pdctMonth
  , pdctEnds
  , pdctFirst
  , pdctLast
  , pdctNth

  , MA.OptSpec ["and"] "" . MA.NoArg . fmap return
    $ addOperand Exp.opAnd

  , MA.OptSpec ["or"] "" . MA.NoArg . fmap return
    $ addOperand Exp.opOr

  , MA.OptSpec ["not"] "" . MA.NoArg . fmap return
    $ addOperand Exp.opNot

  , MA.OptSpec ["open"] "" . MA.NoArg . fmap return
    $ addOperand Exp.openParen

  , MA.OptSpec ["close"] "" . MA.NoArg . fmap return
    $ addOperand Exp.closeParen

  ]

-- | Given a series of numbers, and a designated number that is the
-- beginning of the series, compares two numbers.
compareSeries
  :: Ord a
  => a
  -- ^ The designated beginning of the series
  -> a
  -> a
  -> Ordering
compareSeries b x y = case (compare x b, compare y b) of
  (EQ, LT) -> LT
  (EQ, GT) -> LT
  (LT, EQ) -> LT
  (LT, GT) -> GT
  (GT, EQ) -> GT
  (GT, LT) -> GT
  _ -> compare x y

pdctDate :: MA.OptSpec (ParseOpts -> Ex.Exceptional String ParseOpts)
pdctDate = MA.OptSpec ["date"] "d" (MA.TwoArgE f)
  where
    f a1 a2 = do
      cmpFn <- getComparer a1
      ds <- parsecMultiarg pDateSpec a2
      let g o = do
            let curr = pCurrent o
            day <- Ex.fromMaybe ("invalid date: " ++ a2)
                   $ dayDateSpec curr ds
            let cmp i = compare (iDay i) day
                tok = Exp.operand
                      $ cmpFn (X.pack . show $ day) "date" cmp
                o' = addOperand tok o
            return o'
      return g

getComparer
  :: String
  -> Ex.Exceptional MA.OptArgError
     (Text -> Text -> (a -> Ordering) -> Pd.Pdct a)
getComparer s = Ex.fromMaybe (MA.ErrorMsg "could not parse comparer")
                $ Pd.parseComparerBy (X.pack s)

pdctWeekday :: MA.OptSpec (ParseOpts -> Ex.Exceptional String ParseOpts)
pdctWeekday = MA.OptSpec ["weekday"] "w" (MA.TwoArgE f)
  where
    f a1 a2 = do
      cmpFn <- getComparer a1
      dow <- Ex.fromMaybe (MA.ErrorMsg "bad day of week")
             $ matchDayOfWeek a2
      let g o =
            let cmp a = compareSeries (pWeekStart o)
                        (dayToDayOfWeek . iDay $ a) dow
                pdct = cmpFn (X.pack . show $ dow) "day of week"
                       cmp
            in return $ addOperand (Exp.operand pdct) o
      return g

dayToDayOfWeek :: T.Day -> DayOfWeek
dayToDayOfWeek d
  | w == 1 = Mon
  | w == 2 = Tue
  | w == 3 = Wed
  | w == 4 = Thu
  | w == 5 = Fri
  | w == 6 = Sat
  | w == 7 = Sun
  | otherwise = error "dayToDayOfWeek: toWeekDate failed"
  where
    (_, _, w) = toWeekDate d

pdctDay :: MA.OptSpec (ParseOpts -> Ex.Exceptional String ParseOpts)
pdctDay = MA.OptSpec ["day"] "" (MA.TwoArgE f)
  where
    f a1 a2 = do
      cmpFn <- getComparer a1
      (DayOrLast dayOrLast) <- parsecMultiarg pDayOrLast a2
      let itemDesc = case dayOrLast of
            Left (Day ds) -> X.pack . (show :: Int -> String)
                             . digitsToInt $ ds
            Right _ -> "the last day of the month"
          typeDesc = "day number"
          cmp a = compare aDay bDay
            where
              aDay = let (_, _, day) = T.toGregorian (iDay a)
                     in day
              bDay = case dayOrLast of
                Left (Day ds) -> digitsToInt ds
                Right _ -> let (y, m, _) = T.toGregorian (iDay a)
                           in T.gregorianMonthLength y m
          tok = Exp.operand
                $ cmpFn itemDesc typeDesc cmp
      return $ fmap return (addOperand tok)

pdctYear :: MA.OptSpec (ParseOpts -> Ex.Exceptional String ParseOpts)
pdctYear = MA.OptSpec ["year"] "y" (MA.TwoArgE f)
  where
    f a1 a2 = do
      cmpFn <- getComparer a1
      yr <- MA.reader a2
      let itemDesc = X.pack . show $ yr
          typeDesc = "year number"
          cmp a = compare aYear yr
            where
              aYear = let (y, _, _) = T.toGregorian (iDay a)
                      in y
          tok = Exp.operand $ cmpFn itemDesc typeDesc cmp
      return $ fmap return (addOperand tok)

pdctMonth :: MA.OptSpec (ParseOpts -> Ex.Exceptional String ParseOpts)
pdctMonth = MA.OptSpec ["month"] "m" (MA.TwoArgE f)
  where
    f a1 a2 = do
      cmpFn <- getComparer a1
      mo <- parsecMultiarg pMonth a2
      let itemDesc = X.pack . show $ mo
          typeDesc = "month"
          cmp a = compare aMo mo
            where
              aMo = let (_, m, _) = T.toGregorian (iDay a)
                    in intToMonth m
          tok = Exp.operand $ cmpFn itemDesc typeDesc cmp
      return $ fmap return (addOperand tok)

intToMonth :: Int -> Month
intToMonth i = case i of
  { 1 -> Jan; 2 -> Feb; 3 -> Mar; 4 -> Apr;  5 -> May;  6 -> Jun;
    7 -> Jul; 8 -> Aug; 9 -> Sep; 10 -> Oct; 11 -> Nov; 12 -> Dec;
    _ -> error "intToMonth: bad number" }

pdctEnds :: MA.OptSpec (ParseOpts -> Ex.Exceptional String ParseOpts)
pdctEnds = MA.OptSpec ["ends"] "" (MA.NoArg (fmap return f))
  where
    f = addOperand tok
    tok = Exp.operand pdct
    pdct = begin ||| end
    begin = Pd.operand "first date in the list" ((== 0) . iFwd)
    end = Pd.operand "last date in the list" ((== 0) . iBack)

pdctFirst :: MA.OptSpec (ParseOpts -> Ex.Exceptional String ParseOpts)
pdctFirst = MA.OptSpec ["first"] "" (MA.NoArg (fmap return f))
  where
    f = addOperand
        . Exp.operand
        . Pd.operand "first date in the list"
        $ ((== 0) . iFwd)

pdctLast :: MA.OptSpec (ParseOpts -> Ex.Exceptional String ParseOpts)
pdctLast = MA.OptSpec ["last"] "" (MA.NoArg (fmap return f))
  where
    f = addOperand
        . Exp.operand
        . Pd.operand "last date in the list"
        $ ((== 0) . iBack)

pdctNth :: MA.OptSpec (ParseOpts -> Ex.Exceptional String ParseOpts)
pdctNth = MA.OptSpec ["nth"] "" (MA.OneArgE f)
  where
    f a1 = do
      count <- MA.reader a1
      let g = addOperand
              . Exp.operand
              . Pd.operand (X.pack $ "every " ++ show count ++ " day")
              $ ((== 0) . (`mod` count) . iFwd)
      return (fmap return g)

errExit :: String -> IO a
errExit s = do
  pn <- MA.getProgName
  IO.hPutStrLn IO.stderr $ pn ++ ": error: " ++ s
  Exit.exitFailure

parseOptsInfos :: Opts -> IO (ParseOpts, [DayInfo])
parseOptsInfos os = do
  as <- MA.simpleWithHelp help MA.Intersperse
        (map (fmap Left) allOpts) Right
  today <- fmap (T.localDay . T.zonedTimeToLocalTime) T.getZonedTime
  let (opts, args) = partitionEithers as
      poInit = initParseOpts today os
  po <- Ex.switch errExit return
        . ($ poInit)
        $ foldl' (>=>) return opts
  days <- Ex.switch errExit return
          $ parseArgs (pWeekStart po) (pBase po) (pCurrent po) args
  let infos = dayInfos days
  return (po, infos)

main :: IO ()
main = do
  (po, infos) <- parseOptsInfos myOpts
  pdct <-
    if null (pOperands po)
    then return Pd.always
    else Ex.switch (errExit . X.unpack) return
         $ Exp.parseExpression (pExprDesc po) (pOperands po)
  t <- R.smartTermFromEnv (pColorToFile po) IO.stdout
  let printer = R.printChunks t 
  when (pShowExpression po) $ do
    putStr $ "Filter expression:\n\n"
    printer $ Pd.showPdct 2 0 pdct
    putStr "\n"
  let doEval x = Pd.evaluate 2 True x 0 pdct
      pairs = map doEval infos
  when (pVerboseFilter po) $ do
    putStr $ "Verbose evaluation:\n\n"
    printer . concatMap snd $ pairs
    putStr "\n"
  let remover (info, mayBool) =
        if mayBool == Just True
        then Just (iDay info)
        else Nothing
      days = catMaybes . map remover $ zip infos (map fst pairs)
  mapM_ putStrLn (map (T.formatTime defaultTimeLocale (pFormat po)) days)
