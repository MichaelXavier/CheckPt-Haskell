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
                         clearCollections,
                         clearCollection,
                         clearCollectionItems,
                         clearItem,
                         rootNames,
                         collectionNames,
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
                  where flist = filter (itemMatch n)

lookupCollection :: DataSet -> String -> Maybe MC.MediaCollection
lookupCollection ds n = case flist $ collections ds of
                    x:xs -> Just x
                    _    -> Nothing
                  where flist = filter ((==n) . MC.name)

itemExists :: DataSet -> String -> Bool
itemExists ds s = isJust $ lookupItem ds s

collectionExists :: DataSet -> String -> Bool
collectionExists ds s = isJust $ lookupCollection ds s

clearCollections :: DataSet -> DataSet
clearCollections ds = ds { collections = map MC.complete $ collections ds }

clearCollection :: DataSet -> String -> DataSet
clearCollection ds n = transformCollection (MC.complete) ds n

clearCollectionItems :: DataSet -> String -> [String] -> DataSet
clearCollectionItems ds cn ins = transformCollection (flip MC.clearItems ins) ds cn

clearItem :: DataSet -> String -> DataSet
clearItem ds n = transformItem MI.complete ds n

rootNames :: DataSet -> [String]
rootNames ds = is ++ cs
               where is   = map (qWrap . MI.name) $ items ds
                     cs   = map (qWrap . MC.name) $ collections ds

collectionNames :: DataSet -> String -> [String]
collectionNames ds n = case lookupCollection ds n of
                         Just c -> map (qWrap . MI.name) $ MC.items c
                         _      -> []

--Utilities
extractDataSetPath :: Config -> FilePath
extractDataSetPath = dataSetPath . dataPath

itemMatch :: String -> MI.MediaItem -> Bool
itemMatch n = ((==n) . MI.name)

--TODO: see if this can be applied elsewhere
collectionMatch :: String -> MC.MediaCollection -> Bool
collectionMatch n = ((==n) . MC.name)

-- Could do with some polymporhism here but reassembling it into the DS isn't possible
transformItem :: (MI.MediaItem -> MI.MediaItem) -> DataSet -> String -> DataSet
transformItem t ds n = case break (itemMatch n) $ items ds of
                         (_, [])     -> error $ "Could not find item " ++ n
                         (h, (x:xs)) -> ds { items = h ++ (t x):xs }

transformCollection :: (MC.MediaCollection -> MC.MediaCollection) -> DataSet -> String -> DataSet
transformCollection t ds n = case break (collectionMatch n) $ collections ds of
                         (_, [])     -> error $ "Could not find item " ++ n
                         (h, (x:xs)) -> ds { collections = h ++ (t x):xs }

qWrap :: String -> String
qWrap s = '"':s ++ "\""
