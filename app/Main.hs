{-#LANGUAGE OverloadedStrings
          , NoImplicitPrelude
          #-}

module Main where

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


newtype Keybindings = Keybindings {unKB :: T.Text}

{- TODO use RIO monad
-}
main :: IO ()
main = let kb = Keybindings $ "dud" in welcome >> app kb
 where
  app :: Keybindings -> IO ()
  app kb = do
    response <- printAvailableActions kb
    if response == "t"
      then do
        time <- getCurrentTime
        tPrintLn $ tshow $ toGregorian $ utctDay time
      else do
        tPrintLn $ T.concat ["You wrote:", response]
    app kb


printAvailableActions :: Keybindings -> IO T.Text
printAvailableActions _ = do
  tPrintLn "press 1 to se information about today"
  tPrintLn "Press any other key to quit.."
  response <- getLine
  return $ T.pack response

-- Get information about today

welcome :: IO ()
welcome = tPrintLn "Welcome to planner."

{-
  get current date = getCurrentTime >>= return . toGregorian . utctDay

  https://www.stackbuilders.com/blog/haskell-time-tutorial/
  https://en.wikipedia.org/wiki/Common_Era
  https://two-wrongs.com/haskell-time-library-tutorial.html
  https://wiki.haskell.org/Time#A_time_cheatsheet
  
-}
