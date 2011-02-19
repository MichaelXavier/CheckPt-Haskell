{-# LANGUAGE DeriveDataTypeable #-}   
module CheckPt.DataSet ( DataSet(..), 
                         readDataSet,
                         writeDataSet,
                         writeIfSuccess,
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
                         unclearCollection,
                         deleteCollection,
                         clearCollectionItems,
                         unclearCollectionItems,
                         deleteCollectionItems,
                         clearItem,
                         unclearItem,
                         deleteItem,
                         rootNames,
                         collectionNames,
                         dataSetPath ) where

import System.FilePath ((</>))

import Text.JSON.Generic (decodeJSON, encodeJSON, Data, Typeable)

import Maybe (isJust)
import Data.List (intercalate)
import qualified Data.ByteString.Char8 as Str

import qualified CheckPt.MediaCollection as MC
import qualified CheckPt.MediaItem as MI
import CheckPt.Config (Config(..))
import CheckPt.CLI.Result (Result(..))

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

writeIfSuccess :: Config -> Result DataSet -> IO ()
writeIfSuccess c (Success ds) = writeDataSet c ds
writeIfSuccess _ (Failure f) = putStrLn $ "Error: " ++ f

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
itemExists ds n = isJust $ lookupItem ds n

collectionExists :: DataSet -> String -> Bool
collectionExists ds n= isJust $ lookupCollection ds n

clearCollections :: DataSet -> Result DataSet
clearCollections ds = Success $ collectionsFold MC.complete ds

unclearCollections :: DataSet -> Result DataSet
unclearCollections ds = Success $ collectionsFold MC.uncomplete ds

clearCollection :: DataSet -> String -> Result DataSet
clearCollection = transformCollection (MC.complete)

unclearCollection :: DataSet -> String -> Result DataSet
unclearCollection = transformCollection (MC.uncomplete)

deleteCollection :: DataSet -> String -> Result DataSet
deleteCollection ds n = Success ds { collections = filter ( not . collectionMatch n) $ collections ds}

clearCollectionItems :: DataSet -> String -> [String] -> Result DataSet
clearCollectionItems ds cn ins = transformCollection (flip MC.clearItems ins) ds cn

unclearCollectionItems :: DataSet -> String -> [String] -> Result DataSet
unclearCollectionItems ds cn ins = transformCollection (flip MC.unclearItems ins) ds cn

deleteCollectionItems :: DataSet -> String -> [String] -> Result DataSet
deleteCollectionItems ds cn ins = transformCollection (flip MC.deleteItems ins) ds cn

clearItem :: DataSet -> String -> Result DataSet
clearItem = transformItem MI.complete

unclearItem :: DataSet -> String -> Result DataSet
unclearItem = transformItem MI.uncomplete

deleteItem :: DataSet -> String -> Result DataSet
deleteItem ds n = Success $ ds { items = filter ( not . itemMatch n) $ items ds}

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
transformItem :: (MI.MediaItem -> MI.MediaItem) -> DataSet -> String -> Result DataSet
transformItem t ds n = case break (itemMatch n) $ items ds of
                         (_, [])     -> Failure $ "Could not find item " ++ n
                         (h, (x:xs)) -> Success $ ds { items = h ++ (t x):xs }

transformCollection :: (MC.MediaCollection -> MC.MediaCollection) -> DataSet -> String -> Result DataSet
transformCollection t ds n = case break (collectionMatch n) $ collections ds of
                         (_, [])     -> Failure $ "Could not find collection " ++ n
                         (h, (x:xs)) -> Success $ ds { collections = h ++ (t x):xs }

collectionsFold :: (MC.MediaCollection -> MC.MediaCollection) -> DataSet -> DataSet
collectionsFold t ds = ds { collections = map t $ collections ds }

qWrap :: String -> String
qWrap s = '"':s ++ "\""
