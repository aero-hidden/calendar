module Calendar
  ( Calendar(..)
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

-}

data TimeGran = DAY | WEEK | MONTH | YEAR

span :: Interval UTCTime -> TimeGran
span = undefined

newtype Plan = Plan (T.Text, UTCTime) deriving (Eq, Show)

activity :: Plan -> T.Text
activity (Plan (a, _)) = a

atTime :: Plan -> UTCTime
atTime (Plan (_, t)) = t


newtype Calendar = Calendar [Plan] deriving (Eq, Show)


newtype DailyCal   = DailyCal   [Plan]
newtype MonthlyCal = MonthlyCal [Plan]
newtype WeeklyCal  = WeeklyCal  [Plan]



-- | get plan
(.@<) :: UTCTime -> Calendar -> [Plan]
time .@< (Calendar p) = filter (\plan -> atTime plan == time) p

-- | put plan
(.@>) :: Calendar -> Plan -> Calendar
(Calendar ps) .@> p = Calendar $ p : ps


