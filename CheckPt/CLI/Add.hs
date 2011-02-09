module CheckPt.CLI.Add (execute) where

import CheckPt.CLI.Mode (Mode(..))
import CheckPt.Config (Config(..))

execute :: Mode -> Config -> IO ()
execute _ _ = return undefined
