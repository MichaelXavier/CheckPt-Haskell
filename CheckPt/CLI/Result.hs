module CheckPt.CLI.Result (Result(..)) where

import CheckPt.Config (Config(..))

data Result a = Success a
              | Failure String
