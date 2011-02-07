module CheckPt.Config ( Config(..), defaultConfig ) where

import System.Directory ( getHomeDirectory )

data Config = Config { dataPath :: FilePath }

defaultConfig :: IO Config
defaultConfig = do path <- getHomeDirectory 
                   return Config { dataPath = path }
