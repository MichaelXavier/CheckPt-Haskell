module CheckPt.MediaCollection ( MediaCollection(..), push ) where

{-# LANGUAGE DeriveDataTypeable #-}   

import Text.JSON
import Text.JSON.Generic

import CheckPt.MediaItem hiding (name)

data MediaCollection = MediaCollection { name :: String,
                                         items :: [MediaItem]
                                       } deriving (Eq, Show, Data, Typeable)

push :: MediaCollection -> MediaItem -> MediaCollection
push mc mi = mc { items = mi:(items mc) }
