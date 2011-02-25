{-# LANGUAGE DeriveDataTypeable #-}   
module CheckPt.CLI.Mode (Mode(..)) where

import Data.Typeable(Typeable)
import Data.Data    (Data    )

import CheckPt.Config (Config)

data Mode
  = Add            { name :: String, completed :: Bool                  } 
  | List           { rootonly :: Bool                                   } 
  | Collection     { cname :: String , inames :: [String]               } 
  | Complete       { name :: String , inames :: [String], clear :: Bool } 
  | Uncomplete     { name :: String , inames :: [String], clear :: Bool } 
  | Delete         { name :: String , inames :: [String], clear :: Bool } 
  | GarbageCollect {                                                    } 
  | Names          { toplevel :: String                                 }
  | Init           { force :: Bool																			}
  deriving (Show, Typeable, Data)
