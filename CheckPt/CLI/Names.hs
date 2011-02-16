module CheckPt.CLI.Names (execute) where

import CheckPt.CLI.Mode (Mode(..))
import CheckPt.Config (Config(..))
import CheckPt.DataSet (readDataSet,
                        writeDataSet,
                        rootNames,
                        collectionNames,
                        pushItem)
import qualified CheckPt.MediaItem as MI (MediaItem(..))
import Data.List (intercalate)

execute :: Mode -> Config -> IO ()
execute m c
          | null $ toplevel m = readDataSet c >>= putStrLn . format . rootNames
          | otherwise         = readDataSet c >>= putStrLn . format . (flip collectionNames (toplevel m))
          where format = intercalate " "
