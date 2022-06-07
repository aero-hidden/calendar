module Calendar
  ( Interval
  , minI
  , maxI
  , TimeFormat
  , Calendar(..)
  , Event(..)
  , Longitude
  , Latitude
  , Position
  , (...)
  , isDisj
  , availAt
  , eb4s
  , Intervals
  ) where

import           RIO.Seq                        ( Seq(Empty) )
import qualified RIO.Text                      as TXT
import qualified RIO.Time                      as T
import           RIO.Time                       ( UTCTime )
import GHC.Base (UnliftedType)
import RIO (Utf8Builder)

data Calendar = Calendar
  { events     :: [TimedEvent]
  , timeFormat :: TimeFormat
  , name       :: TXT.Text
  }

data TimeFormat  = AM
                 | PM
                 | TWENTYFOUR deriving (Eq, Show)

newtype TimedEvent = TE { unTimedEvent :: (T.UTCTime,Event) }

data Event = Event
  { title    :: TXT.Text
  , desc     :: TXT.Text
  , at       :: Maybe Position
  , blocking :: Bool -- Can someone plan something here?
  } | EmptyEvent

newtype Longitude = Longitude Int
newtype Latitude = Latitude Int
newtype Position = Position (Longitude,Latitude)
newtype Interval a = Interval (a,a)
newtype Intervals a = Intervals [Interval a]

(...) :: Ord a => a -> a -> Interval a
(...) p1 p2 | p1 <= p2 = Interval (p1, p2)
(...) p1 p2            = Interval (p2, p1)

minI :: Interval a -> a
minI (Interval (a, _)) = a

maxI :: Interval a -> a
maxI (Interval (_, b)) = b

eb4s :: (Ord a) => Interval a -> Interval a -> Bool
eb4s larger smaller = minI larger >= maxI smaller

-- Since cal we dont care about endpoints
isDisj :: (Ord a) => Interval a -> Interval a -> Bool
isDisj i1 i2 = eb4s i1 i2 && eb4s i2 i1

availAt :: Calendar -> Interval a -> [Interval a]
availAt = undefined

class AsSet s where
  (//) :: s -> s -> s
  u :: s -> s -> s
  n :: s -> s -> s
  sum :: [s] -> s
  s :: s -> s -> s


instance Ord a => Monoid (Intervals a) where
  mempty = Intervals []

instance (Ord a ) => Semigroup (Intervals a) where
  (<>) (Intervals i1) (Intervals i2) = Intervals $ i1 <> i2

instance Foldable Intervals where
  foldr = undefined
