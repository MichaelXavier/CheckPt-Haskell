module CheckPt.MediaItem ( MediaItem ) where

{-# LANGUAGE DeriveDataTypeable #-}   

import Text.JSON
import Text.JSON.Generic

data MediaItem = MediaItem { name :: String,
                             completed :: Bool 
                           } deriving (Eq, Show, Data, Typeable)
