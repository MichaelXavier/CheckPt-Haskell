{-# LANGUAGE DeriveDataTypeable #-}   
module CheckPt.MediaCollection ( MediaCollection(..), 
                                 push,
                                 complete,
                                 uncomplete,
                                 completed,
                                 garbageCollect,
                                 unclearItems,
                                 deleteItems,
                                 clearItems ) where

import Text.JSON
import Text.JSON.Generic

import Data.List (intercalate)

import qualified CheckPt.MediaItem as MI

data MediaCollection = MediaCollection { name :: String,
                                         items :: [MI.MediaItem]
                                       } deriving (Eq, Data, Typeable)

push :: MediaCollection -> MI.MediaItem -> MediaCollection
push mc mi = mc { items = mi:(items mc) }

complete :: MediaCollection -> MediaCollection
complete = itemsFold MI.complete

uncomplete :: MediaCollection -> MediaCollection
uncomplete = itemsFold MI.uncomplete

completed :: MediaCollection -> Bool
completed mc = all MI.completed $ items mc

garbageCollect :: MediaCollection -> MediaCollection
garbageCollect mc = mc { items = filter (not . MI.completed) $ items mc}

clearItems :: MediaCollection -> [String] -> MediaCollection
clearItems = foldl clearItem

unclearItems :: MediaCollection -> [String] -> MediaCollection
unclearItems = foldl unclearItem

deleteItems :: MediaCollection -> [String] -> MediaCollection
deleteItems mc ns = mc { items = filter notIn $ items mc }
									  where notIn i = not $ any (flip itemMatch i) ns

instance Show MediaCollection where
  show mc = join $ filter (not . null) [(name mc), is]
            where  sep     =  "\n  "
                   is      = join $ map show $ items mc
                   join    = intercalate sep 

-- Utilities
clearItem :: MediaCollection -> String -> MediaCollection
clearItem = transformItem MI.complete

unclearItem :: MediaCollection -> String -> MediaCollection
unclearItem = transformItem MI.uncomplete

-- REFACTOR
transformItem :: (MI.MediaItem -> MI.MediaItem) -> MediaCollection -> String -> MediaCollection
transformItem t mc n = case break (itemMatch n) $ items mc of
                         (_, [])     -> error $ "Could not find item " ++ n
                         (h, (x:xs)) -> mc { items = h ++ (t x):xs }

itemsFold :: (MI.MediaItem -> MI.MediaItem) -> MediaCollection -> MediaCollection
itemsFold t mc = mc { items = map t $ items mc }

-- REFACTOR
itemMatch :: String -> MI.MediaItem -> Bool
itemMatch n = ((==n) . MI.name)
