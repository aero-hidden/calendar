{-#LANGUAGE OverloadedStrings #-}
module Lib
  ( someFunc
  ) where

import           RIO
import qualified RIO.Text                      as T


someFunc :: IO ()
someFunc = putStrLn "someFunc"

{- main datatype
   a calendar is some pairing of time with events
   we only need events
-}