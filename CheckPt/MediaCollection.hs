module CheckPt.MediaCollection ( MediaCollection(..), push ) where

{-# LANGUAGE DeriveDataTypeable #-}   

import Text.JSON
import Text.JSON.Generic

import Data.List (intercalate, intersperse)

import CheckPt.MediaItem hiding (name)

data MediaCollection = MediaCollection { name :: String,
                                         items :: [MediaItem]
                                       } deriving (Eq, Data, Typeable)

push :: MediaCollection -> MediaItem -> MediaCollection
push mc mi = mc { items = mi:(items mc) }

instance Show MediaCollection where
  show mc = join $ filter (not . null) [(name mc), is]
            where  sep     =  "\n\t"
                   is      = join $ map show $ items mc
                   join    = intercalate sep 
