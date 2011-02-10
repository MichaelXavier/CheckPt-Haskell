{-# LANGUAGE DeriveDataTypeable #-}   
module CheckPt.DataSet ( DataSet(..), 
                         readDataSet,
                         writeDataSet,
                         parseDataSet,
                         stringify,
                         pushItem,
                         pushCollection,
                         lookupItem,
                         lookupCollection,
                         itemExists,
                         collectionExists,
                         dataSetPath ) where

import System.FilePath ((</>))

import Text.JSON
import Text.JSON.Generic

import Maybe (isJust)
import Data.List (intercalate)
import qualified Data.ByteString.Char8 as Str

import qualified CheckPt.MediaCollection as MC
import qualified CheckPt.MediaItem as MI
import CheckPt.Config (Config(..))

data DataSet = DataSet { collections :: [MC.MediaCollection],
                         items :: [MI.MediaItem] 
                       } deriving (Eq, Data, Typeable)

instance Show DataSet where
  show ds = join is mcs 
            where sep        = "\n"
                  join xs ys = intercalate sep $ xs ++ ys
                  is         = map show $ items ds
                  mcs        = map show $ collections ds


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

pushItem :: DataSet -> MI.MediaItem -> DataSet
pushItem ds mi 
            | itemExists ds $ MI.name mi = error "Name taken"
            | otherwise                  = ds { items = mi:(items ds) }

pushCollection :: DataSet -> MC.MediaCollection -> DataSet
pushCollection ds mc 
            | itemExists ds $ MC.name mc = error "Name taken"
            | otherwise                  = ds { collections = mc:(collections ds) }

lookupItem :: DataSet -> String -> Maybe MI.MediaItem
lookupItem ds n = case flist $ items ds of
                    x:xs -> Just x
                    _    -> Nothing
                  where flist = filter ((==n) . MI.name)

lookupCollection :: DataSet -> String -> Maybe MC.MediaCollection
lookupCollection ds n = case flist $ collections ds of
                    x:xs -> Just x
                    _    -> Nothing
                  where flist = filter ((==n) . MC.name)

itemExists :: DataSet -> String -> Bool
itemExists ds s = isJust $ lookupItem ds s

collectionExists :: DataSet -> String -> Bool
collectionExists ds s = isJust $ lookupCollection ds s

--Utilities
extractDataSetPath :: Config -> FilePath
extractDataSetPath = dataSetPath . dataPath
