module CheckPt.CLI.Init (execute) where

import System.Directory ( doesFileExist )

import CheckPt.CLI.Mode (Mode(..))
import CheckPt.Config (Config(..))
import CheckPt.DataSet (DataSet(..), writeDataSet, dataSetPath)

execute :: Mode -> Config -> IO ()
execute m c = (doesFileExist $ dataSetPath $ dataPath c) >>= maybeWrite
							where	maybeWrite exists = case (force m || not exists) of
																					True -> writeDataSet c defaultDS
																					_    -> putStrLn "File already exists. Use -f to overwrite"
										where defaultDS = DataSet { items = [], collections = []}
