module CheckPt.CLI ( modes, dispatch, Mode(..) ) where

import Data.Typeable(Typeable)
import Data.Data    (Data    )

import qualified System.Console.CmdArgs as Arg
import           System.Console.CmdArgs((+=),Annotate((:=)),(&=))

import CheckPt.Config (Config(..), defaultConfig)
import CheckPt.CLI.Mode (Mode(..))
import CheckPt.CLI.Add            as CAdd (execute)
import CheckPt.CLI.List           as CList (execute)
import CheckPt.CLI.Collection     as CCollection (execute)
import CheckPt.CLI.Complete       as CComplete (execute)
import CheckPt.CLI.Uncomplete     as CUncomplete (execute)
import CheckPt.CLI.Names          as CNames (execute)
import CheckPt.CLI.Delete         as CDelete (execute)
import CheckPt.CLI.GarbageCollect as CGarbageCollect (execute)
import CheckPt.CLI.Init           as CInit (execute)


-- Valid modes for checkpt executable:
-- add, list, ... more to come
modes :: Annotate Arg.Ann
modes  = Arg.modes_  [add,
											list,
											collection,
											complete,
											uncomplete,
											names,
											delete,
											gc,
											init]
      += Arg.program "checkpt"
      += Arg.summary "checkpt: track your consumption of media"
      += Arg.help    "Run checkpt help SUBCOMMAND to get more info on one of the subcommands listed below."
  where
  add = Arg.record Add {name = Arg.def, inames = Arg.def, completed = Arg.def}
    [name := error "Must specify an item or collection name"
          += Arg.argPos 0
          += Arg.typ "ITEM_OR_COLLECTION",
     inames := []
           += Arg.args
           += Arg.typ "ITEM_NAMES_IF_COLLECTION",
     completed := False]
    += Arg.help "With 1 argument, adds root level item. With 2, adds item to a collection"
  list = Arg.record List { rootonly = Arg.def }
    [rootonly := False
              += Arg.help "Only list root level items"]
    += Arg.help "Display your list"
    += Arg.auto
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
  gc = Arg.record GarbageCollect { }
     []
     += Arg.name "gc"
     += Arg.help "Delete completed line items and collections"
     --TODO: see if there's a way to not document this subcommand
  names = Arg.record Names { toplevel = Arg.def }
    [toplevel := "Used for CLI completion"
          += Arg.args
          += Arg.typ "COLLECTION_NAME"]
  init = Arg.record Init { force = Arg.def }
    [force := False
           += Arg.help "Force overwrite of checkpt file"]
    += Arg.help "Initialize checkpt"

dispatch :: Mode -> IO ()
dispatch m = case m of
  Add {}            -> defaultConfig >>= CAdd.execute m
  List {}           -> defaultConfig >>= CList.execute m
  Collection {}     -> defaultConfig >>= CCollection.execute m
  Complete {}       -> defaultConfig >>= CComplete.execute m
  Uncomplete {}     -> defaultConfig >>= CUncomplete.execute m
  Names {}          -> defaultConfig >>= CNames.execute m
  Delete {}         -> defaultConfig >>= CDelete.execute m
  GarbageCollect {} -> defaultConfig >>= CGarbageCollect.execute m
  Init {}           -> defaultConfig >>= CInit.execute m

-- Utilities
completionOpts = [name := error "Must specify a name"
                       += Arg.argPos 0
                       += Arg.typ "NAME",
                  inames := []
                       += Arg.args
                       += Arg.typ "ITEM_NAMES",
                  clear := False
                       += Arg.help "Operate on ALL collection items"]
