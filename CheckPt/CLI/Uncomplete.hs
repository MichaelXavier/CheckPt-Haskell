module CheckPt.CLI.Uncomplete (execute) where

import CheckPt.CLI.Mode (Mode(..))
import CheckPt.Config (Config(..))
import CheckPt.DataSet (readDataSet,
                        writeDataSet,
                        unclearCollection,
                        unclearItem,
                        unclearCollectionItems)
import qualified CheckPt.MediaItem as MI (MediaItem(..))

execute :: Mode -> Config -> IO ()
-- Uncomplete a whole collection
execute m@Uncomplete { clear = True, inames = [] } c = do ds <- readDataSet c
                                                        writeDataSet c (unclearCollection ds $ name m)
-- Uncomplete an item
execute m@Uncomplete { inames = [] } c = do ds <- readDataSet c
                                          writeDataSet c (unclearItem ds $ name m)
-- Uncomplete item(s) in a collection
execute m c = do ds <- readDataSet c
                 writeDataSet c (unclearCollectionItems ds (name m) (inames m))
