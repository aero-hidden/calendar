{-#LANGUAGE OverloadedStrings
          , NoImplicitPrelude
           , DefaultSignatures
          #-}

module Pretty
  ( Pretty
  ) where


import           Lib
import           RIO
import qualified RIO.Text                      as T
import           RIO.Time                       ( DayOfMonth
                                                , MonthOfYear
                                                , UTCTime(utctDay)
                                                , Year
                                                , getCurrentTime
                                                , toGregorian
                                                )
import           System.IO                      ( getLine )
import           Util

class Show a => Pretty a where
  pretty :: a -> T.Text


class (Show a, Pretty a, Ord a) => ListPretty a where
  
  beforeSym :: a -> T.Text
  beforeSym = sepSym
  
  afterSym  :: a -> T.Text
  afterSym = sepSym
  
  sepSym    :: a -> T.Text

--

