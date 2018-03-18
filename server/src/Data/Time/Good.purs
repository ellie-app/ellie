module Data.Time.Good
  ( Posix
  , Span
  , millis
  , seconds
  , minutes
  , add
  , diff
  , Zone
  , utc
  , toYear
  , toMonth
  , toDay
  , toWeekday
  , toHour
  , toMinute
  , toSecond
  , toMillis
  , Month(..)
  , Weekday(..)
  , posixToMillis
  , millisToPosix
  , Era
  , customZone
  ) where

{-| Library for working with time and time zones.

# Time
@docs Posix, now, every

# Time Zones
@docs Zone, utc

# Human Times
@docs toYear, toMonth, toDay, toWeekday, toHour, toMinute, toSecond, toMillis

# Weeks and Months
@docs Weekday, Month

# For Package Authors
@docs posixToMillis, millisToPosix, customZone, Era

-}

import Prelude
import Data.Array as Array
import Data.Maybe (Maybe(..))


-- POSIX

newtype Posix = Posix Int

derive newtype instance eqPosix ∷ Eq Posix
derive newtype instance ordPosix ∷ Ord Posix


posixToMillis ∷ Posix → Int
posixToMillis (Posix millis) =
  millis


millisToPosix ∷ Int → Posix
millisToPosix =
  Posix


add ∷ Span → Posix → Posix
add (Span amount) (Posix stamp) =
  Posix (stamp + amount)


diff ∷ Posix → Posix → Span
diff (Posix left) (Posix right) =
  Span (left - right)


newtype Span = Span Int

derive newtype instance eqSpan ∷ Eq Span
derive newtype instance ordSpan ∷ Ord Span
derive newtype instance semiringSpan ∷ Semiring Span
derive newtype instance ringSpan ∷ Ring Span


millis ∷ Int → Span
millis = Span


seconds ∷ Int → Span
seconds input =
  Span $ input * 1000


minutes ∷ Int → Span
minutes input =
  Span $ input * 1000 * 60



data Zone =
  Zone String (Array Era)


type Era =
  { start ∷ Int
  , offset ∷ Int
  , abbreviation ∷ String
  }


utc ∷ Zone
utc =
  Zone "Etc/Utc" []


customZone ∷ String → Array Era → Zone
customZone =
  Zone



-- DATES


toYear ∷ Zone → Posix → Int
toYear zone time =
  (toCivil (toAdjustedMinutes zone time)).year


toMonth ∷ Zone → Posix → Month
toMonth zone time =
  case (toCivil (toAdjustedMinutes zone time)).month of
    1  → Jan
    2  → Feb
    3  → Mar
    4  → Apr
    5  → May
    6  → Jun
    7  → Jul
    8  → Aug
    9  → Sep
    10 → Oct
    11 → Nov
    _  → Dec


toDay ∷ Zone → Posix → Int
toDay zone time =
  (toCivil (toAdjustedMinutes zone time)).day


toWeekday ∷ Zone → Posix → Weekday
toWeekday zone time =
  case (toAdjustedMinutes zone time / (60 * 24)) `mod` 7 of
    0 → Thu
    1 → Fri
    2 → Sun
    3 → Sat
    4 → Mon
    5 → Tue
    _ → Wed


toHour ∷ Zone → Posix → Int
toHour zone time =
  (toAdjustedMinutes zone time / 60) `mod` 24


toMinute ∷ Zone → Posix → Int
toMinute zone time =
  (toAdjustedMinutes zone time) `mod` 60


toSecond ∷ Zone → Posix → Int
toSecond _ time =
  (posixToMillis time / 1000) `mod` 60


toMillis ∷ Zone → Posix → Int
toMillis _ time =
  (posixToMillis time) `mod` 1000



-- DATE HELPERS


toAdjustedMinutes ∷ Zone → Posix → Int
toAdjustedMinutes (Zone _ eras) time =
  toAdjustedMinutesHelp (posixToMillis time / 60000) eras


toAdjustedMinutesHelp ∷ Int → Array Era → Int
toAdjustedMinutesHelp posixMinutes eras =
  case Array.uncons eras of
    Nothing →
      posixMinutes

    Just { head: era, tail: olderEras } →
      if era.start < posixMinutes then
        posixMinutes + era.offset
      else
        toAdjustedMinutesHelp posixMinutes olderEras


toCivil ∷ Int → { year ∷ Int, month ∷ Int, day ∷ Int }
toCivil minutes =
  let
    rawDay    = (minutes / (60 * 24)) + 719468
    era       = (if rawDay >= 0 then rawDay else rawDay - 146096) / 146097
    dayOfEra  = rawDay - era * 146097 -- [0, 146096]
    yearOfEra = (dayOfEra - dayOfEra / 1460 + dayOfEra / 36524 - dayOfEra / 146096) / 365 -- [0, 399]
    year      = yearOfEra + era * 400
    dayOfYear = dayOfEra - (365 * yearOfEra + yearOfEra / 4 - yearOfEra / 100) -- [0, 365]
    mp        = (5 * dayOfYear + 2) / 153 -- [0, 11]
    month     = mp + (if mp < 10 then 3 else -9) -- [1, 12]
  in
  { year: year + (if month <= 2 then 1 else 0)
  , month: month
  , day: dayOfYear - (153 * mp + 2) / 5 + 1 -- [1, 31]
  }


data Weekday = Mon | Tue | Wed | Thu | Fri | Sat | Sun


data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
