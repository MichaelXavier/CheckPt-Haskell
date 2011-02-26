{-# LANGUAGE DeriveDataTypeable #-}   
module CheckPt.MediaItem ( MediaItem(..), complete, uncomplete) where

import Text.JSON
import Text.JSON.Generic
import Text.PrettyPrint.ANSI.Leijen

data MediaItem = MediaItem { name :: String,
                             completed :: Bool 
                           } deriving (Eq, Data, Typeable)

complete :: MediaItem -> MediaItem
complete mi = mi { completed = True }

uncomplete :: MediaItem -> MediaItem
uncomplete mi = mi { completed = False }

instance Show MediaItem where
  show = show . checkBox

-- Utilities
checkBox :: MediaItem -> Doc
checkBox mi = hcat insCheck
              where parts = map text ["[", ("] " ++ name mi)]
                    check = case completed mi of
                              True -> dullgreen $ text "X"
                              _    -> text " "
                    insCheck = (head parts):(check:(tail parts))
