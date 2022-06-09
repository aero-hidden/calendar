{-#LANGUAGE OverloadedStrings
          , NoImplicitPrelude
          #-}

module Main where

import qualified Control.Type.Operator         as O
import           Lib
import           RIO
import qualified RIO.Map                       as M
import           RIO.Map
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

data Environment m a = Environment
  { kb :: Map Char (m a)
  , n  :: Char
  }

{- TODO use RIO monad
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

-}



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

{-https://hackage.haskell.org/package/type-operators-0.2.0.0 

fromList :: (Eq k, Hashable k) => [(k, v)] -> HashMap k v

https://wiki.haskell.org/State_Monad
probably want state to simplify 
-}

data Binding = Char :<- IO ()
{-
  convert then 

-}

bindings = ['a' :<- tPrintLn "asd"]

type IoU = IO ()

newtype KB = KB (M.Map Char IoU)

actionOf :: KB -> Char -> IO ()
actionOf (KB m) c = case (M.lookup c m) of
  (Just a) -> a
  _        -> tPrintLn "Keybinding not found!"

main :: IO ()
main = do
  l <- getLine
  case l of
    (c : _) -> actionOf kbs (c) >> main
    _       -> tPrintLn "askldjas" >> main
 where
  kbs = bindingsOf
    [ 'a' :<- tPrintLn "a"
    , 'b' :<- tPrintLn "b"
    , 'c' :<- tPrintLn "c"
    , 'd' :<- tPrintLn "d"
    ]

bindingsOf :: [Binding] -> KB
bindingsOf bs = KB $ fromList (bindingOf <$> bs)

bindingOf :: Binding -> (Char, IO ())
bindingOf (key :<- action) = (key, action)
