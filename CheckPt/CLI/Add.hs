module CheckPt.CLI.Add (execute) where

import CheckPt.CLI.Mode (Mode(..))
import CheckPt.Config (Config(..))

--TODO: consider creating a typeclass?
--TODO: can we constrain to a particular type constructor (Add)?
execute :: Mode -> Config -> IO ()
execute _ _ = return undefined
