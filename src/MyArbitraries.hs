{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NoImplicitPrelude #-}

module MyArbitraries where


import           Calendar
import           RIO
import qualified RIO.Text                      as T
import           RIO.Time                       ( Day
                                                  ( ModifiedJulianDay
                                                  , toModifiedJulianDay
                                                  )
                                                , toGregorian
                                                )
import           Test.QuickCheck
import           Test.QuickCheck.Instances.Text
                                               as TE
import           Test.QuickCheck.Instances.Time
                                               as TI


rPlan :: Gen Plan
rPlan = do
  letters    <- choose (0, 32)
  (activity) <- (vector letters :: Gen [T.Text])
  ts         <- arbitrary :: Gen Timestamp
  return $ Plan ("ads", ts)

instance Arbitrary Plan where
  arbitrary = do
    rPlan

rTimestamp :: Gen Timestamp
rTimestamp = do
  r <- choose (1600, 3000)
  return . Timestamp . toGregorian $ ModifiedJulianDay r

instance Arbitrary Timestamp where
  arbitrary = do
    rTimestamp

rCalendar :: Gen Calendar
rCalendar = do
  rInt  <- choose (0, 100)
  plans <- (vector rInt) :: Gen [Plan]
  return $ Calendar plans

instance Arbitrary Calendar where
  arbitrary = do
    rCalendar
