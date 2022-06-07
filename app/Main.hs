{-#LANGUAGE OverloadedStrings
          , NoImplicitPrelude
          #-}

module Main where

import           Control.Arrow                  ( ArrowLoop(loop) )
import           Lib
import           RIO
import qualified RIO.Text                      as T
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


