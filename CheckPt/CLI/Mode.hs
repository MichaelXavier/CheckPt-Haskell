{-# LANGUAGE DeriveDataTypeable #-}   
module CheckPt.CLI.Mode (Mode(..)) where

import Data.Typeable(Typeable)
import Data.Data    (Data    )

import CheckPt.Config (Config)

data Mode
  = Add  { name :: String, completed :: Bool }
  | List { rootonly :: Bool }
  | Collection { cname :: String , inames :: [String] }
  deriving (Show, Typeable, Data)
