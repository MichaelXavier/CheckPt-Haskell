module CheckPt.CLI.Mode (Mode(..), CheckPtMode) where

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

-- Doing this will eliminate the need for separte subcommand files, which is fine
--instance CheckPtMode Mode where
--  execute Foo = ...
