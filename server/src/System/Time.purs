module System.Time where

import Prelude
import Control.Monad.Eff (kind Effect, Eff)

foreign import data TIME ∷ Effect
foreign import _now ∷ ∀ e x. Eff (time ∷ TIME | e) Int


now ∷ ∀ e x. Eff (time ∷ TIME | e) Int
now = _now


millisecond ∷ Int
millisecond = 1


milliseconds ∷ Int
milliseconds = millisecond


second ∷ Int
second = 1000


seconds ∷ Int
seconds = second


minute ∷ Int
minute = 60 * seconds


minutes ∷ Int
minutes = minute


hour ∷ Int
hour = 60 * minutes


hours ∷ Int
hours = hour


day ∷ Int
day = 24 * hours


days ∷ Int
days = day


week  ∷ Int
week = 7 * days


weeks  ∷ Int
weeks = week


year ∷ Int
year = 365 * days


years ∷ Int
years = year
