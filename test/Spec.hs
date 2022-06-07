{-#LANGUAGE OverloadedStrings #-}
{-#LANGUAGE NoImplicitPrelude #-}
import           Calendar
import           Prelude                        ( putStrLn
                                                , repeat
                                                )
import           RIO                            ( ($)
                                                , Bool
                                                , Eq((==))
                                                , IO
                                                , Int
                                                , Monad((>>))
                                                , Ord((<), (<=), (>=))
                                                , not
                                                , undefined
                                                , (||)
                                                )
import           RIO
import           RIO.NonEmpty                   ( head )
import qualified RIO.Text                      as T
import           RIO.Time
import           Test.QuickCheck                ( Testable
                                                , quickCheck
                                                )
import           Test.QuickCheck.Instances.Text
import           Util
main :: IO ()
main = calendarTests >> kbTests


{-
   commented quickcheck
-}
cqc :: Testable prop => T.Text -> prop -> IO ()
cqc t p = do
  tPrintLn t
  quickCheck p
  tPrintLn "-"

withTitle :: T.Text -> IO ()
withTitle t = do
  tPrintLn ""
  tPrintLn "===================================="
  tPrintLn $ T.concat ["Initiating testsuite for:", " ", t]
  tPrintLn "===================================="

{-
        calendarstuff
-}
calendarTests :: IO ()
calendarTests =
  withTitle "Calendar"
    >> cqc "1. minI <= maxI" prop_minLOEQ
    >> cqc
         ( T.concat
         $ [ "2. when intervals like : \n"
           , "------I1------ \n"
           , "                   ------I2------  \n"
           , "------I2------ \n"
           , "                   ------I1------  \n"
           , "then true otherwise false"
           ]
         )
         prop_disj
    >> cqc "3. ib4s works" prop_ib4s
--    >> cqc "4. returns one when feed identicals"

prop_ib4s :: Int -> Int -> Bool
prop_ib4s i1 i2 = i1 == i2 || eb4s (i1 ... i1) (i2 ... i2) || i1 < i2

prop_disj :: Int -> Int -> Int -> Int -> Bool
prop_disj p1 p2 p3 p4 =
  let i1 = p1 ... p2
  in  let i2 = p3 ... p4
      in  let bothLess = maxI i1 <= minI i2
          in  let bothLarger = minI i1 >= maxI i2
              in  not bothLess || not bothLarger || isDisj i1 i2

prop_minLOEQ :: Int -> Int -> Bool
prop_minLOEQ i1 i2 = let i = i1 ... i2 in minI i <= maxI i

{-
prop_intervals_disj :: [Int] -> Int -> Bool
prop_intervals_disj is _ | isEven is = undefined

-}
{-
prop_monoid_intervalSet_disj :: UTCTime -> UTCTime -> Int -> Bool
prop_monoid_intervalSet_disj p1 p2 int | int > 0 || int < 0 =
  let evenAmtOf =
        (take (2 * (abs int)) $ repeat (p1 ... p2)) :: [Interval UTCTime]
  in  1 == (length (fold evenAmtOf))
prop_monoid_intervalSet_disj _ _ _ = True
-}

{- =============== 
   KeybindingStuff
  =============== -}

kbTests :: IO ()
kbTests =
  withTitle "Keybinding"
    >> cqc
         (T.concat
           [ "when existing binding pressed"
           , " then show corresponding action"
           , " Otherwise nothing"
           ]
         )
         prop_actionOf_displays_actionOfKeybinding

prop_actionOf_displays_actionOfKeybinding :: T.Text -> Bool
prop_actionOf_displays_actionOfKeybinding txt = undefined
