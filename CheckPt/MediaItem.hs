module CheckPt.MediaItem ( MediaItem ) where

data MediaItem = MediaItem { name :: String,
                             completed :: Bool } deriving (Show)
