module CheckPt.DataSet ( DataSet(..), parse, dataSetPath ) where

{-# LANGUAGE DeriveDataTypeable #-}   

import System.FilePath ((</>))

import Text.JSON
import Text.JSON.Generic

import CheckPt.MediaCollection hiding (items)
import CheckPt.MediaItem
import CheckPt.Config (Config(..))

data DataSet = DataSet { collections :: [MediaCollection],
                         items :: [MediaItem] 
                       } deriving (Eq, Show, Data, Typeable)

--FIXME: needs a lift?
read :: Config -> IO (DataSet)
read c =  fmap parse $ readFile $ dataPath c

parse :: String -> DataSet
parse = decodeJSON

stringify :: DataSet -> String
stringify = encodeJSON

dataSetPath :: FilePath -> FilePath
dataSetPath = flip (</>) (".checkpt.json")
