module CheckPt.CLI.Add (execute) where

import CheckPt.CLI (Add(..))
import CheckPt.Config (Config(..))

--TODO: consider creating a typeclass?
--TODO: actually do something
execute :: Add -> Config -> IO ()
execute _ _ = return undefined
