module CheckPt.CLI.GarbageCollect (execute) where

import qualified CheckPt.CLI.Mode as M (Mode(..))
import CheckPt.Config (Config(..))
import CheckPt.DataSet (DataSet, readDataSet, writeDataSet, garbageCollect)

execute :: M.Mode -> Config -> IO ()
execute _ c = do ds <- readDataSet c
                 writeDataSet c $ garbageCollect ds
