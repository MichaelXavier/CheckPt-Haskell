module CheckPt.CLI.Complete (execute) where

import CheckPt.CLI.Mode (Mode(..))
import CheckPt.Config (Config(..))
import CheckPt.DataSet (readDataSet,
                        writeDataSet,
                        pushItem,
                        clearCollection,
                        clearItem,
                        clearCollectionItems)
import qualified CheckPt.MediaItem as MI (MediaItem(..))

execute :: Mode -> Config -> IO ()
-- Complete a whole collection
execute m@Complete { clear = True, inames = [] } c = do ds <- readDataSet c
                                                        writeDataSet c (clearCollection ds $ name m)
-- Complete an item
execute m@Complete { inames = [] } c = do ds <- readDataSet c
                                          writeDataSet c (clearItem ds $ name m)
-- Complete item(s) in a collection
execute m c = do ds <- readDataSet c
                 writeDataSet c (clearCollectionItems ds (name m) (inames m))
