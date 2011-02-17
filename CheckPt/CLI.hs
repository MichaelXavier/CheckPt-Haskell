module CheckPt.CLI ( modes, dispatch, Mode(..) ) where

import Data.Typeable(Typeable)
import Data.Data    (Data    )

import qualified System.Console.CmdArgs as Arg
import           System.Console.CmdArgs((+=),Annotate((:=)),(&=))

import CheckPt.Config (Config(..), defaultConfig)
import CheckPt.CLI.Mode (Mode(..))
import CheckPt.CLI.Add as CAdd (execute)
import CheckPt.CLI.List as CList (execute)
import CheckPt.CLI.Collection as CCollection (execute)
import CheckPt.CLI.Complete as CComplete (execute)
import CheckPt.CLI.Uncomplete as CUncomplete (execute)
import CheckPt.CLI.Names as CNames (execute)
import CheckPt.CLI.Delete as CDelete (execute)


-- Valid modes for checkpt executable:
-- add, list, ... more to come
modes :: Annotate Arg.Ann
modes  = Arg.modes_  [add,
											list,
											collection,
											complete,
											uncomplete,
											names,
											delete]
      += Arg.program "checkpt"
      += Arg.summary "checkpt: track your consumption of media"
      += Arg.help    "TODO:"
  where
  add = Arg.record Add {name = Arg.def, completed = Arg.def}
    [name := error "Must specify a name"
          += Arg.argPos 0
          += Arg.typ "NAME",
     completed := False]
    += Arg.help "Add a root level item to your list"
  list = Arg.record List { rootonly = Arg.def }
    [rootonly := False
              += Arg.help "Only list root level items"]
    += Arg.help "Display your list"
  collection = Arg.record Collection { cname = Arg.def, inames = Arg.def }
    [cname := error "Must specify a name"
          += Arg.argPos 0
          += Arg.typ "NAME",
     inames := []
          += Arg.args
          += Arg.typ "ITEM_NAMES"]
     += Arg.help "Add or list a collection"
  --TODO: make empty items and clear exclusive if possible
  complete = Arg.record Complete { name = Arg.def, inames = Arg.def, clear = Arg.def }
     completionOpts 
     += Arg.help "Mark an item, collection or items in a collection as complete"
  uncomplete = Arg.record Uncomplete { name = Arg.def, inames = Arg.def, clear = Arg.def }
     completionOpts 
     += Arg.help "Mark an item, collection or items in a collection as incomplete"
  delete = Arg.record Delete { name = Arg.def, inames = Arg.def, clear = Arg.def }
     completionOpts 
     += Arg.help "Delete an item, collection or items"
     --TODO: see if there's a way to not document this subcommand
  names = Arg.record Names { toplevel = Arg.def }
    [toplevel := ""
          += Arg.args
          += Arg.typ "COLLECTION_NAME"]

-- Utilities
dispatch :: Mode -> IO ()
dispatch m = case m of
  Add {}        -> defaultConfig >>= CAdd.execute m
  List {}       -> defaultConfig >>= CList.execute m
  Collection {} -> defaultConfig >>= CCollection.execute m
  Complete {}   -> defaultConfig >>= CComplete.execute m
  Uncomplete {} -> defaultConfig >>= CUncomplete.execute m
  Names {}      -> defaultConfig >>= CNames.execute m
  Delete {}     -> defaultConfig >>= CDelete.execute m

completionOpts = [name := error "Must specify a name"
                       += Arg.argPos 0
                       += Arg.typ "NAME",
                  inames := []
                       += Arg.args
                       += Arg.typ "ITEM_NAMES",
                  clear := False
                       += Arg.help "Mark ALL collection items complete"]
