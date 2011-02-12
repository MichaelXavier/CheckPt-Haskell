module CheckPt.CLI.List (execute) where

import Data.List (intercalate)

import CheckPt.CLI.Mode (Mode(..))
import CheckPt.Config (Config(..))
import CheckPt.DataSet (DataSet, readDataSet, items)

execute :: Mode -> Config -> IO ()
execute List { rootonly = True} c = showItems c
execute List { } c = showAll c

-- Utilities
showAll :: Config -> IO ()
showAll c = readDataSet c >>= putStrLn . show

showItems :: Config -> IO ()
showItems c = readDataSet c >>= putStrLn . formatItems . items
              where formatItems is = intercalate "\n" $ map show is
