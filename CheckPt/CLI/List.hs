module CheckPt.CLI.List (execute) where

import Data.List (intercalate)

import qualified CheckPt.CLI.Mode as M (Mode(..))
import CheckPt.Config (Config(..))
import CheckPt.DataSet (DataSet, readDataSet, items)

execute :: M.Mode -> Config -> IO ()
execute M.List { M.rootonly = True} c = showItems c
execute M.List { } c = showAll c

-- Utilities
showAll :: Config -> IO ()
showAll c = readDataSet c >>= putStrLn . show

showItems :: Config -> IO ()
showItems c = readDataSet c >>= putStrLn . formatItems . items
              where formatItems is = intercalate "\n" $ map show is
