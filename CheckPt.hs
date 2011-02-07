--
-- Copyright (c) 2011 Michael Xavier - http://michaelxavier.net
-- GPL version 2 or later (see http://www.gnu.org/copyleft/gpl.html)
--
 
-- CheckPt Specific
module CheckPt (
  module CheckPt.CLI,
  module CheckPt.CLI.Add,
  module CheckPt.DataSet,
  module CheckPt.MediaCollection,
  module CheckPt.MediaItem
) where

import qualified CheckPt.CLI
import qualified CheckPt.CLI.Add
import qualified CheckPt.DataSet
import qualified CheckPt.MediaCollection
import qualified CheckPt.MediaItem

import CheckPt.CLI (modes, dispatch)

import qualified System.Console.CmdArgs as Arg

main :: IO ()
main = Arg.cmdArgs_ modes >>= dispatch
