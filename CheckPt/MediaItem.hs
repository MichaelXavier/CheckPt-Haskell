{-# LANGUAGE DeriveDataTypeable #-}   
module CheckPt.MediaItem ( MediaItem(..), complete, uncomplete) where

import Text.JSON
import Text.JSON.Generic

data MediaItem = MediaItem { name :: String,
                             completed :: Bool 
                           } deriving (Eq, Data, Typeable)

complete :: MediaItem -> MediaItem
complete mi = mi { completed = True }

uncomplete :: MediaItem -> MediaItem
uncomplete mi = mi { completed = False }

instance Show MediaItem where
  show mi
    | completed mi = "[X] " ++ name mi
    | otherwise    = "[ ] " ++ name mi
