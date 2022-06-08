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
                                                , Year
                                                )
newtype Plan = Plan (T.Text, Timestamp) deriving (Eq, Show)

activity :: Plan -> T.Text
activity (Plan (a, _)) = a

atTime :: Plan -> Timestamp
atTime (Plan (_, t)) = t

newtype Timestamp = Timestamp (Year, MonthOfYear,DayOfMonth) deriving (Eq, Show)

newtype Calendar = Calendar [Plan] deriving (Eq, Show)


-- | get plan
(.@<) :: Timestamp -> Calendar -> [Plan]
time .@< (Calendar p) = filter (\plan -> atTime plan == time) p

-- | put plan
(.@>) :: Calendar -> Plan -> Calendar
(Calendar ps) .@> p = Calendar $ p : ps


