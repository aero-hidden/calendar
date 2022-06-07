{-#LANGUAGE OverloadedStrings
          , NoImplicitPrelude
          #-}
module Util
  ( tPrintLn
  , tPrint
  , tGetLine
  ) where



import           Lib
import           Prelude                        ( putStr
                                                , putStrLn
                                                )
import           RIO
import qualified RIO.Text                      as T
import           System.IO                      ( getLine )


-- | Print message to console
tPrint :: T.Text -> IO ()
tPrint t = putStr $ T.unpack t

-- | Print message to console, end with /n 
tPrintLn :: T.Text -> IO ()
tPrintLn t = putStrLn $ T.unpack t

tGetLine :: IO T.Text
tGetLine = do
  r <- getLine
  return $ T.pack r
