module Calendar
  ( Calendar(..)
  , Timestamp(..)
  , Plan(..)
  , (.@<)
  , (.@>)
  , activity
  , atTime
  ) where

import           GHC.Base                       ( UnliftedType )
import           RIO                            ( Utf8Builder )
import           RIO.Seq                        ( Seq(Empty) )
import qualified RIO.Text                      as T
import           RIO.Time                       ( DayOfMonth
                                                , MonthOfYear
                                                , UTCTime
                                                , Year, TimeOfDay 
                                                )
import Interval

{-
data TimeSign = TS TimeGran Timestamp Timestamp

-}

data TimeGran = DAY | WEEK | MONTH | YEAR

span :: Interval Timestamp -> TimeGran
span = undefined

newtype Plan = Plan (T.Text, Timestamp) deriving (Eq, Show)

activity :: Plan -> T.Text
activity (Plan (a, _)) = a

atTime :: Plan -> Timestamp
atTime (Plan (_, t)) = t

newtype Timestamp = Timestamp (Year, MonthOfYear,DayOfMonth, TimeOfDay) deriving (Eq, Show)

newtype Calendar = Calendar [Plan] deriving (Eq, Show)


newtype DailyCal   = DailyCal   [Plan]
newtype MonthlyCal = MonthlyCal [Plan]
newtype WeeklyCal  = WeeklyCal  [Plan]



-- | get plan
(.@<) :: Timestamp -> Calendar -> [Plan]
time .@< (Calendar p) = filter (\plan -> atTime plan == time) p

-- | put plan
(.@>) :: Calendar -> Plan -> Calendar
(Calendar ps) .@> p = Calendar $ p : ps


