module CheckPt.CLI.Delete (execute) where

import CheckPt.CLI.Mode (Mode(..))
import CheckPt.Config (Config(..))
import CheckPt.DataSet (readDataSet,
                        writeDataSet,
                        deleteCollection,
                        deleteItem,
                        deleteCollectionItems)
import qualified CheckPt.MediaItem as MI (MediaItem(..))

execute :: Mode -> Config -> IO ()
-- Delete a whole collection
execute m@Delete { clear = True, inames = [] } c = do ds <- readDataSet c
                                                      writeDataSet c (deleteCollection ds $ name m)
-- Delete an item
execute m@Delete { inames = [] } c = do ds <- readDataSet c
                                        writeDataSet c (deleteItem ds $ name m)
-- Delete item(s) in a collection
execute m c = do ds <- readDataSet c
                 writeDataSet c (deleteCollectionItems ds (name m) (inames m))
