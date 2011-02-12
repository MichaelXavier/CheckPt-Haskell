module CheckPt.CLI ( modes, dispatch, Mode(..) ) where

import Data.Typeable(Typeable)
import Data.Data    (Data    )

import qualified System.Console.CmdArgs as Arg
import           System.Console.CmdArgs((+=),Annotate((:=)),(&=))

import CheckPt.Config (Config(..), defaultConfig)
import CheckPt.CLI.Mode (Mode(..))
import CheckPt.CLI.Add as CAdd (execute)
import CheckPt.CLI.List as CList (execute)


-- Valid modes for checkpt executable:
-- add, list, ... more to come
modes :: Annotate Arg.Ann
modes  = Arg.modes_  [add, list]
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

-- Utilities
dispatch :: Mode -> IO ()
dispatch m = case m of
  Add {}  -> defaultConfig >>= CAdd.execute m
  List {} -> defaultConfig >>= CList.execute m
