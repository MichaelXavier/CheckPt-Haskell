module CheckPt.MediaItem ( MediaItem(..), complete, uncomplete) where

{-# LANGUAGE DeriveDataTypeable #-}   

import Text.JSON
import Text.JSON.Generic

data MediaItem = MediaItem { name :: String,
                             completed :: Bool 
                           } deriving (Eq, Show, Data, Typeable)

complete :: MediaItem -> MediaItem
complete mi = mi { completed = True }

uncomplete :: MediaItem -> MediaItem
uncomplete mi = mi { completed = False }
