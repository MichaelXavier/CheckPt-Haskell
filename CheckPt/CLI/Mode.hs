module CheckPt.CLI.Mode (Mode(..)) where

import Data.Typeable(Typeable)
import Data.Data    (Data    )

data Mode
  = Add  { name :: String, completed :: Bool }
  | List {}
  deriving (Show, Typeable, Data)
