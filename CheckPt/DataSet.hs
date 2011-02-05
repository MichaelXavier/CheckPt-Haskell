module CheckPt.DataSet ( DataSet, parse ) where

{-# LANGUAGE DeriveDataTypeable #-}   

import Text.JSON
import Text.JSON.Generic

import CheckPt.MediaCollection
import CheckPt.MediaItem

data DataSet = Dataset { collections :: [MediaCollection],
                         items :: [MediaItem] 
                       } deriving (Eq, Show, Data, Typeable)

parse :: String -> DataSet
parse = decodeJSON

stringify :: Dataset -> String
stringify = encodeJSON
