module CheckPt.CLI.Add (execute) where

import CheckPt.CLI.Mode (Mode(..))
import CheckPt.Config (Config(..))
import CheckPt.DataSet (readDataSet, writeDataSet, pushItem)
import qualified CheckPt.MediaItem as MI (MediaItem(..))

--TODO: this is too simple, it only supports adding root items
--FIXME: compiles but file shows as locked
execute :: Mode -> Config -> IO ()
execute m c = do ds <- readDataSet c
                 writeDataSet c (pushItem ds mi)
              where mi = MI.MediaItem { MI.name = name m, MI.completed = completed m }
