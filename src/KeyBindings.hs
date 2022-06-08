
{-#LANGUAGE OverloadedStrings
          , NoImplicitPrelude
          #-}

module KeyBindings where

import           Lib
import           RIO
import qualified RIO.Text                      as T
import           Util

data Action = CalendarOn


newtype Keybindings
    = Keybindings
    [
    (T.Text
    , Maybe Action
    )
    ]


actionOf = undefined

{-
    laskldjaksldjalksjd
-}

