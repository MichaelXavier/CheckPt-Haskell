module CheckPt.DataSet ( DataSet(..), readDataSet, parseDataSet, stringify, dataSetPath ) where

{-# LANGUAGE DeriveDataTypeable #-}   

import System.FilePath ((</>))

import Text.JSON
import Text.JSON.Generic

import Data.List (intercalate)

import CheckPt.MediaCollection hiding (items)
import CheckPt.MediaItem
import CheckPt.Config (Config(..))

data DataSet = DataSet { collections :: [MediaCollection],
                         items :: [MediaItem] 
                       } deriving (Eq, Data, Typeable)

--FIXME: needs a lift?
readDataSet :: Config -> IO (DataSet)
readDataSet c =  fmap parseDataSet $ readFile $ dataPath c

parseDataSet :: String -> DataSet
parseDataSet = decodeJSON

stringify :: DataSet -> String
stringify = encodeJSON

dataSetPath :: FilePath -> FilePath
dataSetPath = flip (</>) (".checkpt.json")

instance Show DataSet where
  show ds = join is mcs 
            where sep        = "\n"
                  join xs ys = intercalate sep $ xs ++ ys
                  is         = map show $ items ds
                  mcs        = map show $ collections ds
