module CheckPt.DataSet ( DataSet(..), 
                         readDataSet,
                         writeDataSet,
                         parseDataSet,
                         stringify,
                         pushItem,
                         pushCollection,
                         dataSetPath ) where

{-# LANGUAGE DeriveDataTypeable #-}   

import System.FilePath ((</>))

import Text.JSON
import Text.JSON.Generic

import Data.List (intercalate)
import qualified Data.ByteString.Char8 as Str

import CheckPt.MediaCollection hiding (items)
import CheckPt.MediaItem
import CheckPt.Config (Config(..))

data DataSet = DataSet { collections :: [MediaCollection],
                         items :: [MediaItem] 
                       } deriving (Eq, Data, Typeable)

readDataSet :: Config -> IO (DataSet)
readDataSet c =  fmap parseDataSet str
                 where path = extractDataSetPath c
                       str  = fmap Str.unpack $ Str.readFile path

writeDataSet :: Config -> DataSet -> IO ()
writeDataSet c ds = writeFile (extractDataSetPath c) (stringify ds)

parseDataSet :: String -> DataSet
parseDataSet = decodeJSON

stringify :: DataSet -> String
stringify = encodeJSON

dataSetPath :: FilePath -> FilePath
dataSetPath = flip (</>) (".checkpt")

extractDataSetPath :: Config -> FilePath
extractDataSetPath = dataSetPath . dataPath

pushItem :: DataSet -> MediaItem -> DataSet
pushItem ds mi = ds { items = mi:(items ds) }

pushCollection :: DataSet -> MediaCollection -> DataSet
pushCollection ds mc = ds { collections = mc:(collections ds) }

instance Show DataSet where
  show ds = join is mcs 
            where sep        = "\n"
                  join xs ys = intercalate sep $ xs ++ ys
                  is         = map show $ items ds
                  mcs        = map show $ collections ds
