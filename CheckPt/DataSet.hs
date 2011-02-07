module CheckPt.DataSet ( DataSet(..), parse ) where

{-# LANGUAGE DeriveDataTypeable #-}   

import Text.JSON
import Text.JSON.Generic

import CheckPt.MediaCollection hiding (items)
import CheckPt.MediaItem

data DataSet = DataSet { collections :: [MediaCollection],
                         items :: [MediaItem] 
                       } deriving (Eq, Show, Data, Typeable)

parse :: String -> DataSet
parse = decodeJSON

stringify :: DataSet -> String
stringify = encodeJSON
