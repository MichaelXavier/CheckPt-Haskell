module CheckPt.CLI.Add (execute) where

import CheckPt.CLI.Mode (Mode(..))
import CheckPt.Config (Config(..))
import CheckPt.DataSet (readDataSet,
                        writeDataSet,
                        writeIfSuccess,
                        pushItem,
                        appendCollection)
import qualified CheckPt.MediaItem as MI (MediaItem(..))

--TODO: this is too simple, it only supports adding root items
--FIXME: compiles but file shows as locked
execute :: Mode -> Config -> IO ()
-- Adding an item
execute m@Add { inames = [] } c = do ds <- readDataSet c
                                     writeDataSet c (pushItem ds mi)
                                  where mi = buildItem (completed m) (name m)
-- Adding item(s) to a collection
execute m c = do ds <- readDataSet c
                 writeIfSuccess c $ appendCollection ds (name m) mis
              where mis = map (buildItem $ completed m) $ inames m

--Utilities
buildItem :: Bool -> String -> MI.MediaItem
buildItem c n = MI.MediaItem { MI.name = n, MI.completed = c }
