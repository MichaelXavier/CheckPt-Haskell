module CheckPt.CLI ( modes, dispatch, Mode(..) ) where

import Data.Typeable(Typeable)
import Data.Data    (Data    )

import qualified System.Console.CmdArgs as Arg
import           System.Console.CmdArgs((+=),Annotate((:=)),(&=))

import CheckPt.Config (Config(..), defaultConfig)
import CheckPt.CLI.Add (execute) as CAdd


data Mode
  = Add  { name :: String, completed :: Bool }
  | List {}
  deriving (Show, Typeable, Data)

-- Valid modes for checkpt executable:
-- add, list, ... more to come
modes :: Annotate Arg.Ann
modes  = Arg.modes_  [add, list]
      += Arg.program "checkpt"
      += Arg.summary "checkpt: track your consumption of media"
      += Arg.help    "TODO:"
  where
  add = Arg.record Add {name = Arg.def, completed = Arg.def}
    [name := error "Must specify a name",
     completed := False]
    += Arg.help "Add an item to your list"
  list = Arg.record List {}
    []
    += Arg.help "Display your list"

--TODO: figure out routing, need a routing module that looks like
-- Mode -> IO()
dispatch :: Mode -> IO ()
dispatch m = case m of
  Add {}     -> CAdd.execute m defaultConfig
  List {} -> return undefined
