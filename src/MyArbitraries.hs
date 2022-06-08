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
                                                , toGregorian, TimeOfDay
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
  rd <- choose (1600, 3000)
  tof <- (arbitrary :: Gen TimeOfDay)
  let 


  
  return . (Timestamp  tof). toGregorian $ ModifiedJulianDay rd

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
