module CheckPt.CLI.List (execute) where

import CheckPt.CLI.Mode (Mode(..))
import CheckPt.Config (Config(..))
import CheckPt.DataSet (DataSet, readDataSet)

execute :: Mode -> Config -> IO ()
execute _ c = readDataSet c >>= putStrLn . show
