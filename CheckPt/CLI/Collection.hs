module CheckPt.CLI.Collection (execute) where

import CheckPt.CLI.Mode (Mode(..))
import CheckPt.Config (Config(..))
import CheckPt.DataSet (readDataSet,
                        writeDataSet,
                        pushCollection,
                        lookupCollection,
                        collectionExists)
import qualified CheckPt.MediaCollection as MC (MediaCollection(..))
import qualified CheckPt.MediaItem as MI (MediaItem(..))

execute :: Mode -> Config -> IO ()
execute m c = do ds <- readDataSet c
                 case foundMC ds of
                   Just mc -> putStrLn $ show mc
                   _       -> pushAndFlush ds
              where 
                    mcName          = cname m
                    foundMC ds      = lookupCollection ds mcName
                    newI n          = MI.MediaItem {MI.name = n, MI.completed = False}
                    newIs           = map newI $ inames m
                    newMC           = MC.MediaCollection {MC.name = mcName, MC.items = newIs}
                    pushAndFlush ds = writeDataSet c (newDS) >> (putStrLn . show) newDS
                      where newDS = pushCollection ds newMC
