module CheckPt.CLI.Mode (Mode(..)) where

import Data.Typeable(Typeable)
import Data.Data    (Data    )

import CheckPt.Config (Config)

data Mode
  = Add  { name :: String, completed :: Bool }
  | List {}
  deriving (Show, Typeable, Data)

-- Is it possible to attach a type constructor to a typeclass
class CheckPtMode a where
  execute :: a -> Config -> IO ()
