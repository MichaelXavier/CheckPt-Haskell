module CheckPt.MediaCollection ( MediaCollection, push ) where

import CheckPt.MediaItem
import CheckPt.SimpleMediaCollection

--The functions will probbaly clash, which is why you want different modules


data MediaCollection = MediaCollection { name :: String,
                                         items :: [MediaItem]
                                       } deriving (Show)

--TODO: just make it an instance of Enum or something
push :: MediaCollection -> MediaItem -> MediaCollection
push mc mi = mc { items = mi:(items mc) }
