{-# LANGUAGE DeriveDataTypeable #-}   
module CheckPt.MediaCollection ( MediaCollection(..), 
                                 push,
                                 complete,
                                 uncomplete,
                                 unclearItems,
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

clearItems :: MediaCollection -> [String] -> MediaCollection
clearItems = foldl clearItem

unclearItems :: MediaCollection -> [String] -> MediaCollection
unclearItems = foldl unclearItem

instance Show MediaCollection where
  show mc = join $ filter (not . null) [(name mc), is]
            where  sep     =  "\n\t"
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
